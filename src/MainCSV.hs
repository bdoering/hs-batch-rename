-- Batch rename files (e.g. from a Nikon camera) based on naming info
-- provided in a CSV file

{-# LANGUAGE RecordWildCards #-}

import System.FilePath
import System.Directory (renameFile, getModificationTime)
import System.IO.Error
import Options.Applicative
import Control.Monad
import Control.Monad.Trans
import Graphics.HsExif
import Text.Printf
import Data.Time
import Data.String.Utils
import Data.List (groupBy, find, sort, intercalate, sortBy)
import Data.Maybe (maybe, isJust, fromJust)
import Data.Char (isDigit, toLower)
import System.Posix.Files (getFileStatus)
import Control.Monad.Trans.Either
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Map.Strict as M

data Options = Options
    { csvFilename :: FilePath
    , filenames :: [FilePath]
    , prepareCSV :: Bool
    , dryRun :: Bool }

data Image = Image { exifDateTime :: Maybe LocalTime
                   , modificationDateTime :: LocalTime
                   , filepath :: FilePath }
             deriving Show

-- Combine images with a common basename into one group
data ImageGroup = ImageGroup { basename :: String -- ^Filename with path, without extension
                             , groupDateTime :: LocalTime -- ^Representative date
                             , images :: [Image] }
                  deriving Show

instance Eq ImageGroup where
  (==) (ImageGroup b1 _ _) (ImageGroup b2 _ _) = b1 == b2

-- First order by EXIF date, and if not available, then by
-- modification date (first in list)
instance Ord ImageGroup where
  compare x y = compare (groupDateTime x) (groupDateTime y)

imagesDateTime :: [Image] -> LocalTime
imagesDateTime xs = case (find (isJust . exifDateTime) xs) of
        Nothing -> (modificationDateTime . head $ xs)
        Just x -> fromJust $ exifDateTime $ x

readExifDateTimeOriginal :: FilePath -> IO (Maybe LocalTime)
readExifDateTimeOriginal x = parseFileExif x >>=
  either (\_ -> return Nothing) (\x -> return $ getDateTimeOriginal x)

readModificationDateTime :: FilePath -> IO LocalTime
readModificationDateTime x = do
  modificationTimeUtC <- getModificationTime x
  timeZone <- getCurrentTimeZone
  return $ utcToLocalTime timeZone modificationTimeUtC

fmtDate :: LocalTime -> String
fmtDate = showGregorian . localDay

showImage :: Image -> String
showImage (Image e m f) =
  "Exif: " ++ (maybe "(n/a)     " fmtDate e) ++
  "  Modi: " ++ (fmtDate m) ++
  "  Filename: " ++ takeFileName f

readImages :: [FilePath] -> IO [Image]
readImages = mapM readImage

readImage :: FilePath -> IO Image
readImage x = do
  exifDateTime <- readExifDateTimeOriginal x
  modificationDateTime <- readModificationDateTime x
  return Image {filepath=x, ..}

group :: [Image] -> [ImageGroup]
group = map mkImageGroup . groupBy (fOnBasename (==)) . sortBy (fOnBasename compare)
  where
    fOnBasename f (Image _ _ f1) (Image _ _ f2) = f (takeBaseName f1) (takeBaseName f2)
    mkImageGroup i@((Image _ _ f):xs) = ImageGroup (dropExtension f) (imagesDateTime i) i

generateCSVFile :: [ImageGroup] -> BL.ByteString
generateCSVFile gs = Csv.encode $ header ++ toRows gs
  where header = [("Basename", "Timestamp (EXIF or modification time)")]
        toRows = map ((,) <$> basename <*> show . groupDateTime)

renameAll :: Bool -> String -> ImageGroup -> Int -> IO ()
renameAll dryRun basename g n = mapM_ (rename (groupDateTime g)) (images g) 
  where rename gd (Image _ _ p) = do
          let (bn, ext) = splitExtension p
              dir = takeDirectory bn
              newFn = dir </> printf "%s_%03d_%s" (show.localDay$gd) n basename <.> map toLower ext
              dirLen = length dir
          res <- if dryRun
                 then return (Right ())
                 else tryIOError $ renameFile p newFn
          
          case res of
            Left e -> putStrLn $ "*** Caught exception for " ++ p ++ " : " ++
                      show e ++ " (skipping)"
            Right _ -> putStrLn $ concat [
              if dirLen > 20 then ".." ++ drop (dirLen - 20) dir else dir, ": ",
              takeFileName p, " --> ", takeFileName newFn]

newBasename :: [String] -- ^ Previous basename components (assume no empty fields)
            -> [String] -- ^ These basename components
            -> [String] -- ^ Resulting basename components
newBasename (x:xs) ([]:ys) = x:newBasename xs ys
newBasename (x:xs) ("#":ys) = [x]
newBasename _ ys = ys

newBasenames _ [] = []
newBasenames x (y:ys) = new : newBasenames new ys
  where new = newBasename x y

doRename :: Bool -> M.Map String String -> Int -> ImageGroup -> IO ()
doRename dryRun nameMap n group = do
  case M.lookup (basename group) nameMap of
    Nothing -> putStrLn $ "*** No name for group: " ++ basename group
    Just bn -> renameAll dryRun bn group n
  
parseOptions :: Parser Options
parseOptions = Options
               <$> (argument str (metavar "CSVFILE"))
               <*> some (argument str (metavar "FILES"))
               <*> switch
               ( long "prepare-csv"
                 <> help "Only generate a CSV template" )
               <*> switch
               ( long "dryrun"
                 <> help "Do not actually rename the files" )

exit = left

moreThanOne [] = False
moreThanOne (x:[]) = False
moreThanOne (x:_) = True

run :: Options -> IO ()
run (Options c fs prepare dryRun) = do
  e <- runEitherT $ do
    imageFiles <- lift $ readImages fs

    lift $ mapM print imageFiles

    -- Generate one ImageGroup per day; each image group contains files with the same basename
    let imageGroups = groupBy (\x y -> (localDay . groupDateTime) x == (localDay . groupDateTime) y) . sort . group $ imageFiles :: [[ImageGroup]]
    lift $ putStrLn $ "There are " ++ show (length imageFiles) ++ " images and " ++
      show (length imageGroups) ++ " groups (days)"

    liftIO $ putStrLn "-----------------"
    lift $ mapM print $ filter (moreThanOne) $ imageGroups
  

  
    -- Possibly generate a CSV template file before doing anything else
    when prepare ((lift $ BL.writeFile "batch-rename-csv.csv" $ generateCSVFile (concat imageGroups))
                  >> exit ())
    lift . print $ "Done (did not generate a CSV file"

    -- Read CSV as bytestring
    csvText <- lift $ BL.readFile c
    let decoded = Csv.decode Csv.NoHeader csvText :: Either String (V.Vector (V.Vector BL.ByteString))
    case decoded of
      Left err -> lift . putStrLn $ "Error while reading CSV: " ++ err
      Right csv -> do
        lift . putStrLn $ "Number of groups: " ++ show (length imageGroups) ++
          "  Number of CSV lines: " ++ show (V.length csv)
        lift . print $ csv V.! 0
        lift . print $ csv V.! 1
        lift . print $ csv V.! (V.length csv - 1)
        let cleaned = map ((map prepareCell) . V.toList) $ V.toList csv
            (c:csvBasenames) = map (drop 2) cleaned
            -- basenames = c : (zipWith newBasename (c:csvBasenames) csvBasenames )
            basenames = map (intercalate "_" . filter (/= "") . map (replace " " "-")) $ c : newBasenames c csvBasenames
            nameMap = M.fromList $ zip (map head cleaned) basenames
            numberedImageGroups = map (\g -> zip [1..] g) imageGroups :: [[(Int,ImageGroup)]]
        lift $ mapM_ (\(n,g) -> doRename dryRun nameMap n g) $ concat numberedImageGroups
          
      
  case e of
    Left a -> putStrLn "Terminated prematurely"
    Right b -> putStrLn "Finished the whole thing"

-- Convert a bytestring CSV cell to a cleaned String
prepareCell = filter (/= '"') . strip . show

main = execParser opts >>= run
    where
      opts = info (helper <*> parseOptions)
             ( fullDesc
               <> progDesc "Description..."
               <> header "Header..." )
  

{-# LANGUAGE RecordWildCards #-}

module Lib
       ( readImageGroups
       , renameAll
       ) where

import Data.Char (isDigit, toLower)
import Data.List (groupBy, find, sort, intercalate, sortBy)
import Data.Maybe (maybe, isJust, fromJust)
import Data.Time
import Graphics.HsExif
import System.Directory (renameFile, getModificationTime)
import System.FilePath
import System.IO.Error
import System.Posix.Files (getFileStatus)
import Text.Printf
import qualified Data.Map.Strict as M

import Types


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

readImageGroups :: [FilePath] -> IO [[ImageGroup]]
readImageGroups fn = fmap groupByBNAndDay (readImages fn)

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

-- Rename all files within an ImageGroup (same images with different extensions)
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
            Right _ -> putStrLn $
                       concat [ if dirLen > 20 then ".." ++ drop (dirLen - 20) dir else dir, ": ",
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

moreThanOne [] = False
moreThanOne (x:[]) = False
moreThanOne (x:_) = True

-- Sort images by groups of identical basenames (just extension is
-- different), and sort those groups by day
groupByBNAndDay :: [Image] -> [[ImageGroup]]
groupByBNAndDay = groupBy (\x y -> (localDay . groupDateTime) x ==
                                   (localDay . groupDateTime) y) .
                  sort . group 


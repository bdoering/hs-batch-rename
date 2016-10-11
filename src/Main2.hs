-- Batch rename files (e.g. from a Nikon camera) based on a common
-- basename (typically prepended with the EXIF or modification date)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.String.Utils
import Options.Applicative
import qualified Data.ByteString.Lazy as BL

import Types
import Lib

data Options = Options
    { commonName :: FilePath
    , filenames  :: [FilePath]
    , nodate     :: Bool
    , dryRun     :: Bool }

parseOptions :: Parser Options
parseOptions = Options
               <$> (argument str (metavar "BASENAME"))
               <*> some (argument str (metavar "FILES"))
               <*> switch
               ( long "nodate"
                 <> short 'n'
                 <> help "Do not prepend basename with EXIF (or modification) date" )
               <*> switch
               ( long "dryrun"
                 <> help "Do not actually rename the files" )



run :: Options -> IO ()
run (Options bn fs nodate dryRun) = do
  e <- runEitherT $ do
    -- imageFiles <- lift $ readImages fs

    -- lift $ mapM print imageFiles

    -- Generate one ImageGroup per day; each image group contains files with the same basename
    imageGroups <- lift $ readImageGroups fs
    lift $ putStrLn $ "There are " ++ show (length imageGroups) ++ " groups (days)"

    liftIO $ putStrLn "-----------------"
    -- lift $ mapM print $ filter (moreThanOne) $ imageGroups
    

      -- let cleaned = map ((map prepareCell) . V.toList) $ V.toList csv
      --       (c:csvBasenames) = map (drop 2) cleaned
      --       -- basenames = c : (zipWith newBasename (c:csvBasenames) csvBasenames )
      --       basenames = map (intercalate "_" . filter (/= "") . map (replace " " "-")) $ c : newBasenames c csvBasenames
      --       nameMap = M.fromList $ zip (map head cleaned) basenames
      --       numberedImageGroups = map (\g -> zip [1..] g) imageGroups :: [[(Int,ImageGroup)]]
      --   lift $ mapM_ (\(n,g) -> doRename dryRun nameMap n g) $ concat numberedImageGroups
          
      
  case e of
    Left a -> putStrLn "Terminated prematurely"
    Right b -> putStrLn "Finished the whole thing"

main = execParser opts >>= run
    where
      opts = info (helper <*> parseOptions)
             ( fullDesc
               <> progDesc "Description..."
               <> header "Header..." )
  

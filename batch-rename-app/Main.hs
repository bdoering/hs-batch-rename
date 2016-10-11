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
run (Options cn fs nodate dryRun) = do
  e <- runEitherT $ do

    -- Generate one ImageGroup per day; each image group contains files with the same basename
    imageGroups <- lift $ readImageGroups fs
    lift $ putStrLn $ concat [ "Found "
                             , show (sum . map (length . images) . concat $ imageGroups)
                             , " images (from "
                             , show (length imageGroups)
                             , " different days)" ]
    -- If date info shall not be exploited, simply concat all day
    -- groups again. This is not the most efficient approach...
    let numberedImageGroups = if nodate
                              then zip [1..] (concat imageGroups)
                              else concat( map (\g -> zip [1..] g) imageGroups)
    lift $ mapM_ (\(n,g) -> renameAll nodate dryRun cn g n) numberedImageGroups
  case e of
    Left a -> putStrLn "Terminated prematurely"
    Right b -> return ()

main = execParser opts >>= run
    where
      opts = info (helper <*> parseOptions)
             ( fullDesc
               <> progDesc "Description..."
               <> header "Header..." )
  

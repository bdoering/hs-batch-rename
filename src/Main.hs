-- Command line utility which allows to rename all specified files to
-- a common basename with an increasing number appended.

import System.FilePath
import System.Directory (doesFileExist, renameFile)
import Data.List
import Data.Function (on)
import Options.Applicative
import Text.Printf (printf)
import Data.Char (toLower)

data Options = Options
             { basename :: String
             , filenames :: [String]
             , dryRun :: Bool }

parseOptions :: Parser Options
parseOptions = Options
               <$> argument str (metavar "BASENAME")
               <*> some (argument str (metavar "FILES"))
               <*> switch
                   ( long "dry-run"
                     <> help "do not modify filenames if provided" )

filenamesForGroup :: String -> Int -> [(String, String)] -> [String]
filenamesForGroup b n group = map newFilename group
    where
      newFilename (path, ext) = replaceFileName path (printf "%s_%03d" b n) <.> (map toLower ext)

-- Actual program logic
run :: Options -> IO ()
run (Options b fs dryrun) = do
  -- Generate groups of common base names with increasing numbers
  let groups = zip [0..] . groupBy (\x y -> fst x == fst y) . sort . map splitExtension $ fs
  let newFilenames = concat . map (\e -> filenamesForGroup b (fst e) (snd e)) $ groups
  let oldFilenames = map (\e -> fst e <.> snd e) (concat . map snd $ groups)
  mapM_ (\(x,y) -> renameAction x y dryrun) $ zip oldFilenames newFilenames

renameAction :: String -> String -> Bool -> IO ()
renameAction old new dryrun = do
  putStr $ old ++ " --> " ++ new
  if dryrun
  then do putStrLn " (skipped)" 
  else do renameFile old new
          putStrLn " (done)"
           
main :: IO ()
main = execParser opts >>= run
    where
      opts = info (helper <*> parseOptions)
             ( fullDesc
               <> progDesc "Batch rename files with a common basename and a successive serial number (one number for all files with identical basenames but different extensions). "
               <> header "A clever batch renamer!" )

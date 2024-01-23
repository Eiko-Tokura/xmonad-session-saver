module Main where

import System.Environment
import System.Posix.Process
import System.Path
import System.Directory

unJust (Just a) = a

data ProgramInfo = ProgramInfo
  { workingDir :: String
  , pName      :: String
  , arguments  :: [String]
  , pidStr     :: String
  , workspace  :: Maybe String
  , saveStatus :: Bool
  } deriving (Show, Read)

main = do
  home  <- getHomeDirectory
  workd <- getCurrentDirectory
  name  <- getProgName
  args  <- getArgs
  pid   <- launch name args
  let progInfo = ProgramInfo workd name args (show pid) Nothing False
  appendFile (unJust $ absNormPath home sessionFile) 
             (show progInfo ++ "\n")

sessionFile = ".viewedDocs"

launch prog args = forkProcess $ executeFile ("/usr/bin/" ++ prog) True args Nothing

{-# OPTIONS_GHC -XPatternGuards -XFlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ViewDoc (toggleSaveState, updateSaveState, startUpCleanUp, colorSaved, launchDocuments) where

import Control.Monad
import qualified Data.ByteString.Char8 as Str
import XMonad.Util.ExtensibleState as XS
import qualified Data.Set as S 

import System.Posix.Types
import XMonad hiding (launch)
import XMonad.StackSet (currentTag, allWindows)
import XMonad.Core
import XMonad.Hooks.ManageHelpers
import XMonad.Operations
import XMonad.Actions.SpawnOn
import XMonad.Actions.WorkspaceNames

import XMonad.Prompt
import XMonad.Prompt.Input

import System.FilePath.Posix
import System.Posix.Process
import System.Posix.Files
import System.Directory
import Text.Read (readMaybe)
--import System.Path
import Data.Maybe

history :: String
history = ".viewedDocs"

histdir :: String
histdir = ".xdocs"

savedColor          = "#ff88aa" :: String
savedColorUnfocused = "#beefac" :: String
unsavedColor        = "#66aaff":: String

data ProgramInfo = ProgramInfo
  { workingDir :: String
  , pName      :: String
  , arguments  :: [String]
  , pidStr     :: String
  , workspace  :: Maybe String
  , saveStatus :: Bool
  } deriving (Show, Read)

data Storage = Storage (S.Set ProcessID) deriving (Typeable,Read,Show)
instance ExtensionClass Storage where
   initialValue = Storage S.empty
   extensionType = PersistentExtension

unStorage (Storage s) = s

toggleSaveState :: X ()
toggleSaveState = updateSaveState not

updateSaveState :: (Bool -> Bool) -> X ()
updateSaveState fbool = do
    wsp  <- getCurrentWorkspace
    withFocused (runQuery pid >=> updateDoc fbool (Just wsp))

getCurrentWorkspace = withWindowSet $ return . currentTag

startUpCleanUp :: X ()
startUpCleanUp = do
  withWindowSet (return . allWindows >=> (`emptyList` cleanUp))
  where emptyList [] = id
        emptyList _  = const $ return ()
        cleanUp :: X ()
        cleanUp = io $ Str.readFile history >>= 
          writeFile history . unlines . map show . filter saveStatus . mapMaybe readMaybe . lines . Str.unpack

updateDoc :: (Bool -> Bool) -> Maybe String -> Maybe ProcessID -> X ()
updateDoc fbool mwsp t = case t of
    Nothing -> return ()
    Just p ->  do 
      historyStr <- io $ Str.readFile history
      let listProgInfo  = mapMaybe readMaybe $ filter (/= "") $ lines $ Str.unpack historyStr
          listProgInfo' = map (togglePid fbool p mwsp) listProgInfo
      colorWindows p (any (\(ProgramInfo _ _ _ pid _ flag) -> pid == show p && flag) listProgInfo')
      io $ writeFile history (unlines (map show listProgInfo'))

togglePid fbool p mwsp l@(ProgramInfo wd cmd args pid _ flag)
    | show p == pid = ProgramInfo wd cmd args pid mwsp (fbool flag)
    | otherwise     = l

colorWindows :: ProcessID -> Bool -> X ()
colorWindows p True  = do 
  XS.modify (push p)
  withFocused $ \w -> setWindowBorder' savedColor w
    where push p (Storage s) = Storage $ S.insert p s
colorWindows p False = do
  XS.modify (pull p)
  withFocused $ \w -> setWindowBorder' unsavedColor w
    where pull p (Storage s) = Storage $ S.delete p s 

setWindowBorder' :: (MonadIO m, MonadReader XConf m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

colorSaved :: X ()
colorSaved = do
  withFocused   (giveColor savedColor) --withFocused (runQuery pid >=> colorSaved')
  withUnfocused (giveColor savedColorUnfocused)
  where giveColor color w = do
            mPID <- runQuery pid w
            pids <- XS.get
            when (fromMaybe False $ mPID >>= return . (`S.member` unStorage pids)) (setWindowBorder' color w)

launchDocuments :: X ()
launchDocuments = do
  home <- io getHomeDirectory
  wsp <- getCurrentWorkspace
  f <- io $ Str.readFile (home </> history)
  g <- mapM (launchFile (Just wsp)) (lines $ Str.unpack f)
  io $ setCurrentDirectory home
  io $ writeFile history (unlines g)

launchFile :: Maybe String -> String -> X String
launchFile mwsp "" = return ""
launchFile mwsp f  = launchFile' mwsp (read f)
  where launchFile' :: Maybe String -> ProgramInfo -> X String
        launchFile' mwsp l@(ProgramInfo wd cmd args _ wsp flag)
            | flag && mwsp == wsp   = do 
                pid <- io $ launch wd cmd args
                colorWindows pid True
                return $ show (ProgramInfo wd cmd args (show pid) mwsp flag)
            | otherwise             = return $ show l

launch wd prog args = do
  setCurrentDirectory wd 
  forkProcess $ executeFile ("/usr/bin/" ++ prog) True args Nothing

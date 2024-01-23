# xmonad-session-saver
A session saver module used in xmonad, can be used to save simple programs you run in xmonad, like zathura pdf documents you opened.


This is a fork and rework of [xmonad-sessions](https://github.com/zaxtax/xmonad-sessions), the basic ideas are also taken from this repo, it uses a program saveDocuments to act as a proxy to record the commandlines used to launch the specific programs. Bugs are fixed in order to work for xmonad-0.17, and better data structures are used. Time permitting, I would consider rewrite the entire project.


Changes were made, and new functions were added, including:
* Support for recording workspace, and launch documents only launch windows opened in this workspace. You can launch in separate workspaces.
* When you launch documents, already existing windows will not be relaunched.
* Support for recording the working directory where the command is launched.
* Clean up the caches when you start up xmonad.


To use it, 
1. make sure you installed MissingH (cabal install --lib MissingH)
2. then edit the script install.sh, add all apps you want to record (remove the comment in the symbolic links section and add more apps by adding more lines). It works perfectly for zathura, not really working for nvim, nvim-qt. 
````bash
#sudo ln -s /usr/local/bin/saveDocument /usr/local/bin/zathura
````
3. Then run the script install.sh
4. in your xmonad.hs, import the modules, add ewmh before xmonad, and add colorSaved in your logHook.
````haskell
import ViewDoc

main = do
  xmonad . ewmh $ defaults
    { ...
    , logHook = colorSaved <> ...
    } 
````
5. Add key bindings for saving
````haskell
    -- Save sessions
    , ((modm              , xK_s     ), toggleSaveState)
    , ((modm .|. shiftMask, xK_s     ), launchDocuments)
````
6. Add startUpCleanUp to your StartupHook
````haskell
myStartupHook = do
    startUpCleanUp      -- Clean .viewedDocs
    ...
````
7. Recompile you xmonad, and enjoy! You can change the window border colors in ViewDoc.hs as you want.


The different window borders looks great!
![preview](./preview.png)

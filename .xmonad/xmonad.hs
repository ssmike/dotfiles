import System.Environment
import Control.Monad
import XMonad.Config.Desktop
import Data.Monoid;
import XMonad.Hooks.ScreenCorners
import XMonad.Config.Kde
import XMonad.Actions.WindowGo
import XMonad
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Layout.Cross
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Actions.GridSelect
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Mosaic
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.CopyWindow
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Layout.Named
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import qualified XMonad.Layout.BoringWindows as B
import Data.List
import XMonad.Hooks.SetWMName


myTerminal :: String
myTerminal      = "term"

myModMask       = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["1:main","2:web","3:code","4:media","5:FM", "6:work", "7:math", "8:game", "9:etc"]


-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  ="#6A86B2"
myFocusedBorderColor ="#DB2828"

scratchpads = [
    NS "terminal" "xfce4-terminal --role scratchpad"
      (stringProperty "WM_WINDOW_ROLE" =? "scratchpad")
      (customFloating $ W.RationalRect (1/12) (0) (5/6) (1/2)),
    NS "browser" "luakit" (className =? "luakit")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]

myXPConfig = defaultXPConfig {
        bgColor = "#000000"
    ,   fgColor = "#FFFFFF" -- "#5D69B4"
    ,   borderColor = "#3CB424"
    ,   font = "Ubuntu Mono 12"
    ,   height = 20
}
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ --((modm, xK_s), namedScratchpadAction scratchpads "browser")
    -- minimize current window
      ((modm,               xK_m     ), withFocused minimizeWindow)
    -- restore next window
    , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
    -- open terminal scratchpad
    , ((0, xK_F12), namedScratchpadAction scratchpads "terminal")
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      --exit
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm, xK_a), sendMessage ToggleStruts)
    -- launch file manager
    , ((modm, xK_f), spawn "thunar")
    -- go to coresponding workspace
    , ((modm .|. shiftMask, xK_f), windows $ W.greedyView "5:FM")
    , ((modm .|. shiftMask, xK_s), windows $ W.greedyView "6:work")
    , ((modm .|. shiftMask, xK_d), windows $ W.greedyView "7:math")
    -- go to window prompt
    , ((modm .|. shiftMask, xK_o     ), windowPromptGoto  myXPConfig)
    -- bring window prompt
    , ((modm .|. shiftMask, xK_i     ), windowPromptBring  myXPConfig)
    -- open window selection menu
    , ((modm, xK_o), goToSelected defaultGSConfig)
    -- launch command prompt
    , ((modm, xK_p     ), shellPrompt myXPConfig)
    -- launch screensaver
    , ((controlMask .|. shiftMask , xK_l), spawn "slock")
    --close current window
    , ((modm .|. shiftMask, xK_c     ), kill1)
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,              xK_Tab    ), B.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), B.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), B.focusUp  )
    -- Move focus to the master window
    , ((modm .|. shiftMask,   xK_Tab ), B.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_Right), moveTo Next (WSIs notSP))
    , ((modm             , xK_Left), moveTo Prev (WSIs notSP))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    --send window to etc
    ,((modm .|. shiftMask, xK_a), windows $ W.shift "9:etc")
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  [((m .|. modm, k), windows $ f i)
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1 ..]
  , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, controlMask)]]

notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
myLayout = modifiers $  ( onWorkspaces ["9:etc"] (cross ||| Full) $
                            onWorkspaces ["3:code", "6:work", "7:math"]
                                (my_mosaic ||| Full ||| tiled) $
                            onWorkspaces ["2:web", "4:media"]
                                (all_equal ||| Full ||| Mirror tiled) $
                            --["1:main", "5:fm"]
                            all_equal ||| Full ||| my_mosaic
                          )
  where
    modifiers = avoidStruts . minimize . B.boringWindows
    my_mosaic = mosaic 3 [6, 2, 1]
    all_equal = named "Equal" $  mosaic 2 []
    cross = Cross cross_ratio delta
    cross_ratio = 6/7
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 3/4
    delta   = 5/100

myManageHook = (scratchpadManageHook (W.RationalRect 0 0 1 0.4)) <+>
    (composeAll . concat $
    [
      [className =? c --> doIgnore | c <- ignore]
    , [className =? c --> doFullFloat | c <- fullfloat]
    , [className =? c --> doFloat | c <- float]
    , [className =? c --> doShift "4:media" | c <- media]
    , [className =? c --> doShift "5:FM" | c <- fM]
    , [className =? c --> doShift "9:etc" | c <- etc]
    , [resource  =? "desktop_window" --> doIgnore ]
    , [resource  =? "kdesktop"       --> doIgnore ]
    , [className =? c --> doShift "2:web" | c <- web]
    , [className =? c --> doShift "3:code" | c <- code]
    , [className =? c --> doShift "6:work" | c <- work]
    , [className =? c --> doShift "7:math" | c <- math]
    , [className =? c --> doShift "8:game" | c <- game]
    , [stringProperty "WM_WINDOW_ROLE" =? "bubble" --> doIgnore]
--    , [className =? c --> doF W.swapDown | c <- aux]
    , [isDialog --> doFloat]
    ]
    )  <+> manageDocks <+> (namedScratchpadManageHook scratchpads)
    where
        aux = ["kate", "konsole", "Term", "Xfce4-terminal"]
        game = ["Steam", "dota_linux"]
        math = ["TexMaker", "XMaxima", "Wxmaxima", "geogebra-GeoGebra", "XMathematica"]
        work = ["okular", "Okular", "Zathura", "libreoffice", "libreoffice-writer", "libreoffice-calc", "libreoffice-impress", "libreoffice-startcenter", "VCLSalFrame.DocumentWindow", "VCLSalFrame"]
        web = ["Chromium", "chromium-browser-chromium", "Chromium-browser", "Firefox"]
        code = ["jetbrains-idea", "Qvim", "Emacs", "Gvim", "jetbrains-idea-ce", "Codelite", "NetBeans IDE 8.0", "Subl3", "Leksah"]
        fullfloat = ["trayer", "panel"]
        float = ["Kmix", "org.kde.gwenview", "kmix", "Klipper", "ksplashx", "ksplashqml", "ksplashsimple", "Yakuake", "Plasma-desktop", "XTerm", "Tilda", "Blueman-services", "Nm-connection-editor", "Blueman-manager", "Gimp", "mpv", "MPlayer", "Umplayer", "Smplayer", "Vlc", "Gnuplot", "VirtualBox", "Wine", "Gcdemu", "Docky"]
        ignore = ["trayer", "Zenity", "Oblogout"]
        media = ["mpv", "nuvolaplayer3-deezer", "google-music-electron", "Tomahawk", "nuvolaplayer", "Vlc", "MPlayer", "Umplayer", "Smplayer", "Cheese", "Minitube"]
        fM = ["k4dirstat", "krusader", "Pcmanfm", "Dolphin", "Gnome-commander", "Thunar", "Baobab", "Catfish"]
        etc = ["Corebird", "Telegram", "Qbittorrent", "Kmail", "kmail", "Clementine", "Transmission-gtk", "Transmission-qt" ,"Deluge", "Ekiga", "Claws-mail"]

myEventHook e = do
    screenCornerEventHook e
    docksEventHook e

myStartupHook = do
    spawn "~/.xmonad/autostart.sh"
    spawn "~/.xmonad/dzen-auto.sh"
    spawn "xsetroot -cursor_name left_ptr"
    addScreenCorner SCUpperRight (goToSelected defaultGSConfig)
    return ()

myLogHook dzen = do
  dynamicLogWithPP $ dzenpp dzen
  --setWMName "LG3D"

main = do
    dzen <- spawnPipe "/usr/bin/dzen2 -ta l -dock -x 0 -y 0 -e -"
    setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
    xmonad $ ewmh $ kde4Config {
            terminal           = myTerminal,
            focusFollowsMouse  = False,
            borderWidth        = 3,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            keys               = myKeys,
            mouseBindings      = myMouseBindings,
            logHook            = myLogHook dzen,
            layoutHook         = smartBorders $  myLayout,
            manageHook         = myManageHook,
            handleEventHook    = ewmhDesktopsEventHook <+>
                                 ewmhCopyWindow <+>
                                 handleEventHook kde4Config <+>
                                 screenCornerEventHook,
            startupHook        = myStartupHook
        }

--for krunner
kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt

ewmhCopyWindow :: Event -> X All
ewmhCopyWindow ClientMessageEvent {
               ev_window = w,
               ev_message_type = mt,
               ev_data = -1 : _
       } = withWindowSet $ \s -> do
    trace "notifications hack"
    a_cd <- getAtom "_NET_CURRENT_DESKTOP"
    --trace $ "debug for notifications " ++ show mt ++ " " ++ show a_cd
    when (mt == a_cd) $ do
      sort' <- getSortByIndex
      let ws = map W.tag $ sort' $ W.workspaces s
      windows $ foldr (.) id (map (copyWindow w) ws)
      windows W.focusDown
    return (All True)
ewmhCopyWindow _ = return (All True)

newcolor = "#000000"
dzenpp status = defaultPP {
                ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
              , ppCurrent           =   dzenColor "white" newcolor
              , ppVisible           =   dzenColor "blue" newcolor . workspaceClickable
              , ppHidden            =   dzenColor "#A09BA1" newcolor . workspaceClickable
              , ppUrgent            =   dzenColor "#ff0000" newcolor
              , ppWsSep             =   " "
              , ppSep               =   "  " ++ (dzenColor "green" newcolor . dzenEscape $ "\\") ++  "  "
              , ppLayout            =   dzenColor "#A09BA1" newcolor . layoutClickable . deleteMinimize
              , ppTitle             =   (" " ++) . dzenColor "white" newcolor . dzenEscape
              , ppOutput            =   hPutStrLn status
           }
           where
            deleteMinimize s = if "Minimize " `isPrefixOf` s then drop (length "Minimize ") s else s
            layoutClickable s = "^ca(1,xdotool key super+space)" ++ s ++ "^ca()"
            workspaceClickable s = "^ca(1,xdotool key super+" ++ (take 1 s) ++ ")" ++ s ++ "^ca()"


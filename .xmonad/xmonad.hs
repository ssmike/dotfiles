import System.Posix.Env (setEnv)
import qualified System.Environment as E
import Control.Monad
import Data.Monoid;
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
import XMonad.Layout.NoBorders
import XMonad.Layout.Cross
import XMonad.Prompt
import XMonad.Layout.MultiToggle
import qualified XMonad.Layout.MultiToggle.Instances as Toggles
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Mosaic
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.CopyWindow
import XMonad.Layout.Named
import Data.List
import XMonad.Hooks.UrgencyHook
import qualified XMonad.Layout.Tabbed as Tab

myTerminal :: String
myTerminal      = "kitty"

myWorkspaces :: [String]
myWorkspaces = ["1:main","2:web","3:code","4:im","5:fm", "6:doc", "7:dev", "8:low", "9:etc"]

scratchpads :: [NamedScratchpad]
scratchpads = [
    NS "terminal" (myTerminal ++ " --class scratchpad")
      (className =? "scratchpad")
      (customFloating $ W.RationalRect (1/12) 0 (5/6) (1/2))
    ]

myXPConfig = def {
        bgColor = "#000000"
    ,   fgColor = "#FFFFFF" -- "#5D69B4"
    ,   borderColor = "#3CB424"
    ,   height = 24
    ,   font = "-misc-fixed-*-*-*-*-18-*-*-*-*-*-*-*"
}

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [
    -- open terminal scratchpad
    ((0, xK_F12), namedScratchpadAction scratchpads "terminal")
    -- launch a terminal
    , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      --exit
    , ((modm .|. shiftMask, xK_r     ), io exitSuccess)

    -- fullscreen
    , ((modm, xK_a), sendMessage $ Toggle Toggles.NBFULL)

    , ((modm, xK_f), spawn "nautilus")

    -- go to workspace
    , ((modm .|. shiftMask, xK_f), windows $ W.greedyView "5:fm")
    , ((modm .|. shiftMask, xK_s), windows $ W.greedyView "6:doc")
    , ((modm .|. shiftMask, xK_d), windows $ W.greedyView "7:dev")

    -- launch command prompt
    , ((modm, xK_p     ), spawn "dmenu_run")
    , ((modm, xK_n     ), spawn "networkmanager_dmenu")

    -- launch screensaver
    , ((controlMask .|. shiftMask , xK_l), spawn "slock")
    --close current window
    , ((modm .|. shiftMask, xK_c     ), kill1)
    --switch layout
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Move focus to the next window
    , ((modm,              xK_Tab    ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm .|. shiftMask,   xK_Tab ), windows W.focusMaster  )
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
    , ((modm              , xK_Left), moveTo Prev (WSIs notSP))

    , ((modm              , xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    --prompt with windows from current workspace
    , ((modm, xK_o), windowPrompt myXPConfig Bring wsWindows)

    , ((modm .|. shiftMask, xK_h), swapPrevScreen)
    , ((modm .|. shiftMask, xK_l), swapNextScreen)

    , ((modm              , xK_BackSpace), focusUrgent)
    , ((modm .|. shiftMask, xK_BackSpace), clearUrgents)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- mod-ctrl-[1..9], Copy client to workspace N
    --
    -- mod-{q,w,e}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{q,w,e}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
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

myLayout = modifiers $  ( onWorkspaces ["9:etc"] (cross ||| tabbedFull) $
                            onWorkspaces ["3:code", "7:dev"] -- make room for coding
                                (myMosaic ||| tabbedFull ||| horizontal) $
                            --["1:main", "2:web", "4:im", "5:fm", "6:doc", "8:low"]
                            tabbedFull ||| all_equal ||| horizontal
                          )
  where
    modifiers = smartBorders . (mkToggle (Toggles.NBFULL ?? EOT)) . avoidStruts
    myMosaic = mosaic 3 [6, 2, 1]
    all_equal = named "Equal" $  mosaic 2 []
    cross = Cross cross_ratio delta
    cross_ratio = 6/7
    horizontal  = named "Tall" $ Mirror $ Tall nmaster delta ratio
    nmaster = 1
    ratio   = 3/4
    delta   = 5/100
    tabbedFull = named "Tabbed" $ Tab.tabbed Tab.shrinkText $ def {
        Tab.activeColor = "#323234",
        Tab.activeBorderColor = "#323234",
        Tab.inactiveColor = "#0C0C0D",
        Tab.inactiveBorderColor = "#0C0C0D",
        Tab.urgentColor = "#0C0C0D",
        Tab.urgentBorderColor = "#336DFF",
        Tab.decoHeight = 24,
        Tab.fontName = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
    }

myManageHook =
    (composeAll . concat $
    [
      [className =? c --> doIgnore | c <- ignore]
    , [className =? c --> doFullFloat | c <- fullfloat]
    , [className =? c --> doFloat | c <- float]
    , [className =? c --> doShift "4:im" | c <- im]
    , [stringProperty "WM_WINDOW_ROLE" =? "Mutt" --> doShift "4:im"]
    , [stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "2:web"]
    , [className =? c --> doShift "5:fm" | c <- fM]
    , [className =? c --> doShift "9:etc" | c <- etc]
    , [resource  =? "desktop_window" --> doIgnore ]
    , [resource  =? "kdesktop"       --> doIgnore ]
    , [className =? c --> doShift "2:web" | c <- web]
    , [className =? c --> doShift "3:code" | c <- code]
    , [className =? c --> doShift "6:doc" | c <- doc]
    , [className =? c --> doShift "7:dev" | c <- dev]
    , [className =? c --> doShift "8:low" | c <- low]
    , [stringProperty "WM_WINDOW_ROLE" =? "bubble" --> doIgnore]
    , [className =? c --> doF W.swapDown | c <- aux]
    , [isDialog --> doFloat]
    ]
    )  <+> manageDocks <+> (namedScratchpadManageHook scratchpads)
    where
        aux = ["kitty", "kate", "konsole", "Term", "Xfce4-terminal"]
        low = ["Popcorn-Time", "VirtualBox Manager", "vlc", "Steam", "dota_linux", "XCOM: Enemy Within", "mpv", "google-music-electron", "Tomahawk", "Vlc", "MPlayer", "Umplayer", "Smplayer", "Cheese", "Minitube", "PornTime"]
        dev = ["TexMaker", "XMaxima", "Wxmaxima", "geogebra-GeoGebra", "XMathematica"]
        doc = ["Dia", "calibre", "FBReader", "Evince", "Blender", "Gimp", "Gimp-2.8", "Gimp-2.9", "okular", "Okular", "Zathura", "libreoffice", "libreoffice-writer", "libreoffice-calc", "libreoffice-impress", "libreoffice-startcenter", "VCLSalFrame.DocumentWindow", "VCLSalFrame"]
        web = ["Vivaldi-stable", "Vivaldi-snapshot", "orion", "yandex-browser-beta", "Opera", "Chromium-browser-chromium", "Chromium", "chromium-browser-chromium", "Chromium-browser", "Firefox"]
        code = ["jetbrains-pycharm", "NyaoVim", "Code", "jetbrains-pycharm-ce", "jetbrains-clion", "QtCreator", "Pycrust-3.0", "jetbrains-idea", "Qvim", "Emacs", "Gvim", "jetbrains-idea-ce", "Codelite", "NetBeans IDE 8.0", "Subl3", "Leksah"]
        fullfloat = ["trayer", "panel"]
        float = ["Shutter", "Pavucontrol", "Kmix", "org.kde.gwenview", "kmix", "Klipper", "ksplashx", "ksplashqml", "ksplashsimple", "Yakuake", "Plasma-desktop", "XTerm", "Tilda", "Blueman-services", "Nm-connection-editor", "Blueman-manager", "mpv", "MPlayer", "Umplayer", "Smplayer", "Gnuplot", "Wine", "Gcdemu", "Docky"]
        ignore = ["Snapfly", "trayer", "Zenity", "Oblogout"]
        im = ["VK", "skypeforlinux", "Thunderbird", "Pidgin", "Corebird", "Slack", "Telegram", "TelegramDesktop", "Kmail", "kmail", "Claws-mail"]
        fM = ["Nautilus", "k4dirstat", "krusader", "Pcmanfm", "Dolphin", "Gnome-commander", "Thunar", "Baobab", "Catfish"]
        etc = ["nuvolaplayer3-deezer", "qBittorrent", "nuvolaplayer3", "Qbittorrent", "Clementine", "Transmission-gtk", "Transmission-qt" ,"Deluge", "Ekiga"]

myStartupHook = do
    liftIO $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1" True
    liftIO $ setEnv "XDG_CURRENT_DESKTOP" "GNOME" True
    spawn "~/.xmonad/dzen-auto.sh"
    spawn "~/.xmonad/autostart.sh"
    spawn "xsetroot -cursor_name left_ptr"
    return ()

myEventHook =
    ewmhDesktopsEventHook <+>
    ewmhCopyWindow <+>
    (handleEventHook kde4Config)

main = do
    homePath <- E.getEnv "HOME"
    monitor <- readFile $ homePath ++ "/.xmonad/primary_monitor"
    status <- spawnPipe $ "/usr/bin/dzen2 -ta l -dock -x 0 -y 0 -e - -xs " ++ monitor
    let modifiers = (withUrgencyHook NoUrgencyHook) . ewmh
    xmonad $ modifiers $ kde4Config {
            terminal           = myTerminal,
            focusFollowsMouse  = False,
            borderWidth        = 3,
            modMask            = mod3Mask,
            workspaces         = myWorkspaces,
            normalBorderColor  = "#6A86B2",
            focusedBorderColor = "#DB2828",
            keys               = myKeys,
            mouseBindings      = myMouseBindings,
            logHook            = dynamicLogWithPP $ dzenpp status,
            layoutHook         = myLayout,
            manageHook         = myManageHook,
            handleEventHook    = myEventHook,
            startupHook        = myStartupHook
        }

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

dzenpp status = def {
                ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
              , ppCurrent           =   dzenColor "white" newcolor
              , ppVisible           =   dzenColor "blue" newcolor . workspaceClickable
              , ppHidden            =   dzenColor "#A09BA1" newcolor . workspaceClickable
              , ppUrgent            =   dzenColor "#ff0000" newcolor . workspaceClickable
              , ppWsSep             =   " "
              , ppSep               =   "  " ++ (dzenColor "green" newcolor . dzenEscape $ "\\") ++  "  "
              , ppLayout            =   dzenColor "#A09BA1" newcolor
              , ppTitle             =   (" " ++) . dzenColor "white" newcolor . dzenEscape
              , ppOutput            =   hPutStrLn status
           }
           where
            newcolor = "#000000"
            wnum name = (read :: [Char] -> Int) $ take 1 name
            workspaceClickable s = "^ca(1,wmctrl -s " ++ show ((wnum s) - 1) ++ ")" ++ s ++ "^ca()"


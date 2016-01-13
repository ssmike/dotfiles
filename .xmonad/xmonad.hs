import XMonad.Config.Desktop
import Data.Monoid;
import XMonad.Hooks.ScreenCorners
import XMonad.Config.Kde
import XMonad.Actions.WindowGo
import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Layout.Cross
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Accordion
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

myTerminal      = "konsole"
 
myModMask       = mod4Mask
 
myWorkspaces = ["1:main","2:web","3:code","4:media","5:FM", "6:work", "7:math", "8:game", "9:etc"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  ="#6A86B2"
myFocusedBorderColor ="#DB2828"

scratchpads = [
    NS "browser" "luakit" (className =? "luakit") 
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))--,
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
      ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
      --exit
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm, xK_a), sendMessage ToggleStruts)
	, ((modm, xK_f), spawn "thunar")
    , ((modm .|. shiftMask, xK_f), windows $ W.greedyView "5:FM")
    , ((modm .|. shiftMask, xK_s), windows $ W.greedyView "6:work")
    , ((modm .|. shiftMask, xK_d), windows $ W.greedyView "7:math")

    , ((modm .|. shiftMask, xK_o     ), windowPromptGoto  defaultXPConfig)
    , ((modm .|. shiftMask, xK_i     ), windowPromptBring  defaultXPConfig)
    , ((modm, xK_o), goToSelected defaultGSConfig)
    -- launch command prompt
    , ((modm, xK_p     ), shellPrompt myXPConfig)
    
    , ((controlMask .|. shiftMask , xK_l), spawn "slock")
    
    , ((modm .|. shiftMask, xK_c     ), kill1)

    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
     
    -- Move focus to the next window
    , ((modm,              xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
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
   	--[((m .|. modm, k), windows $ f i)
    --    | (i, k) <- zip ["NSP"] [xK_f]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
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
 
myLayout = avoidStruts $  ( onWorkspaces ["9:etc"] (cross ||| Full) $
                            onWorkspaces ["3:code", "6:work", "7:math"] 
                                (my_mosaic ||| Full ||| tiled) $
                            onWorkspaces ["2:web", "4:media"] 
                                (all_equal ||| Full ||| Mirror tiled) $
                            --["1:main", "5:fm"]
                            all_equal ||| Full ||| my_mosaic
                          ) 
  where
    my_mosaic = mosaic 3 [6, 2, 1]
    all_equal = named "Equal" $  mosaic 2 []
    cross = Cross cross_ratio delta
    cross_ratio = 6/7 
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 3/4
    delta   = 5/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
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
    , [className =? c --> doF W.swapDown | c <- aux]
    , [isDialog --> doFloat]
    ]
    )  <+> manageDocks
    where
        aux = ["kate", "konsole", "Term"]
        game = ["Steam", "dota_linux"] 
        math = ["TexMaker", "XMaxima", "Wxmaxima", "geogebra-GeoGebra", "XMathematica"]
        work = ["okular", "Okular", "Zathura", "libreoffice", "libreoffice-writer", "libreoffice-calc", "libreoffice-impress", "libreoffice-startcenter", "VCLSalFrame.DocumentWindow", "VCLSalFrame"]
        web = ["Chromium", "chromium-browser-chromium", "Chromium-browser", "Firefox"]
        code = ["jetbrains-idea", "Qvim", "Emacs", "Gvim", "jetbrains-idea-ce", "Codelite", "NetBeans IDE 8.0", "Subl3", "Leksah"]
        fullfloat = ["trayer", "panel"]
        float = ["Kmix", "org.kde.gwenview", "kmix", "Klipper", "ksplashx", "ksplashqml", "ksplashsimple", "Yakuake", "Plasma-desktop", "XTerm", "Tilda", "Blueman-services", "Nm-connection-editor", "Blueman-manager", "Gimp", "mpv", "MPlayer", "Umplayer", "Smplayer", "Vlc", "Gnuplot", "VirtualBox", "Wine", "Gcdemu", "Docky"]
        ignore = ["trayer", "Zenity", "Oblogout"]
        media = ["mpv", "nuvolaplayer3-deezer", "google-music-electron", "Tomahawk", "nuvolaplayer", "Vlc", "MPlayer", "Umplayer", "Smplayer", "Cheese", "Minitube"]
        fM = ["krusader", "Pcmanfm", "Dolphin", "Gnome-commander", "Thunar", "Baobab", "Catfish"]
        etc = ["Corebird", "Telegram", "Qbittorrent", "Kmail", "kmail", "Clementine", "Transmission-gtk", "Transmission-qt" ,"Deluge", "Ekiga", "Claws-mail"]
 
------------------------------------------------------------------------
-- Event handling
 
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--fullscreenEventHook <+> 
myEventHook e = do
    screenCornerEventHook e
    docksEventHook e

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
--myLogHook = return ()
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = do 
    spawn "wmname LG3D"
    spawn "~/.xmonad/autostart.sh"
    spawn "~/.xmonad/dzen-auto.sh"
    spawn "xsetroot -cursor_name left_ptr"
    --addScreenCorner SCUpperRight (windowPromptGoto  defaultXPConfig)
    return ()
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
-- -w 1020
main = do 
    dzen <- spawnPipe "/usr/bin/dzen2 -ta l -dock -x 0 -y 0 -e -"
    xmonad $ kde4Config {
            terminal           = myTerminal,
            focusFollowsMouse  = False,
            borderWidth        = 3,
            modMask            = myModMask,
            workspaces         = myWorkspaces,
            normalBorderColor  = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
    
            keys               = myKeys,
            mouseBindings      = myMouseBindings,
            logHook            = dynamicLogWithPP $ dzenpp dzen,
    
            layoutHook         = smartBorders $  myLayout,
            manageHook         = (((className =? "krunner") >>= return . not --> myManageHook) <+> (kdeOverride --> doFloat)),
            handleEventHook    = (ewmhDesktopsEventHook `mappend` ewmhCopyWindow) <+> handleEventHook kde4Config,
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
    --let a_cd = 446 :: Word64 --  getAtom "_NET_CURRENT_DESKTOP"
    --trace $ "debug for notifications " ++ show mt ++ " " ++ show a_cd
    --when (mt == a_cd) $ do
    trace "notifications hack"
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
              , ppVisible           =   dzenColor "blue" newcolor
              , ppHidden            =   dzenColor "#A09BA1" newcolor
              , ppUrgent            =   dzenColor "#ff0000" newcolor
              , ppWsSep             =   " "
              , ppSep               =   "  " ++ (dzenColor "green" newcolor . dzenEscape $ "\\") ++  "  "
              , ppLayout            =   dzenColor "#A09BA1" newcolor
              , ppTitle             =   (" " ++) . dzenColor "white" newcolor . dzenEscape
              , ppOutput            =   hPutStrLn status 
           }

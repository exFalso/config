{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as S
import           XMonad.Util.EZConfig         (additionalKeys, removeKeys)
import           XMonad.Util.Run              (spawnPipe)

import           Text.Regex.TDFA
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Writer
import           Data.List
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Monoid
import           System.IO
import           System.Posix                 (ProcessID)

wallpaper :: String
wallpaper = "~/Wallpapers/haskell.jpg"

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , isFullscreen             --> doFullFloat
    ]

type Sync = WriterT [String] X

syncly :: Sync () -> X ()
syncly s = do
  cs <- execWriterT s
  spawn $ intercalate " ; " cs

run :: String -> Sync ()
run = tell . return

unliftX :: ((forall a. X a -> IO a) -> IO b) -> X b
unliftX f = do
  r <- ask
  s <- get
  let ul x = fst <$> runX r s x
  io $ f ul

myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    syncly $ do
      run "echo > $HOME/.xmacro/status"
      -- run "xrandr --output LVDS-0 --auto --right-of VGA-0"
      -- run "xrandr --output LVDS-0 --primary"
      run $ "feh --bg-scale " ++ wallpaper
      run "emacs --daemon"
    return ()

_SMALL_DELAY :: Int
_SMALL_DELAY = 100000

spawnDelay :: String -> X ()
spawnDelay str = do
  spawn str
  liftIO $ threadDelay 100000

main :: IO ()
main = do
  spamToggle <- newMVar False
  xmproc <- spawnPipe "xmobar"
  spawn "killall twmnd ; twmnd"
  let tall = Tall 1 (3/100) (1/2)
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> myManageHook
                      <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ smartBorders tall ||| smartBorders Full ||| smartBorders (Mirror tall)
       , startupHook = myStartupHook
       , logHook = dynamicLogWithPP $ xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
       , modMask = mod4Mask
       , terminal = "urxvt"
       } `removeKeys`
       [ (mod4Mask, xK_q)

       ] `additionalKeys`
       [ ((modShift, xK_Print), spawnDelay "import -window root $HOME/tmp/Screenshot.png")
       , ((0, xK_Print), spawnDelay "screenshot_region")
       , ((modShift, xK_f), wSwitch "chromium") -- "conkeror &> $HOME/.conkeror.mozdev.org/log")
       , ((modShift, xK_s), wSwitch "skype")
       , ((modShift, xK_w), wSwitch "emacs")
       , ((modShift, xK_r), wSwitch "urxvt -e ncmpcpp")
       , ((modShift, xK_apostrophe), spawnDelay "shakemon")
       , ((mod4Mask, xK_p), spawnDelay "dmenu_run -fn 'DejaVu Sans Mono-9'")
       , ((modShift, xK_g), spawnDelay "dmake $HOME/.gamesmake")
       , ((modShift, xK_e), spawnDelay "dmake $HOME/.editmake")
       , ((modShift, xK_d), spawnDelay "dmake $HOME/.displaymake")
       , ((mod4Mask, xK_k), spawnDelay "dkill -15")
       , ((modShift, xK_k), spawnDelay "dkill -9")
       , ((modShift, xK_v), spawnDelay "gnome-volume-control")
       , ((mod4Mask, xK_Home), spawnDelay "amixer set Master 25")
       , ((mod4Mask, xK_End), spawnDelay "amixer set Master 0")
       , ((mod4Mask, xK_Page_Up), spawnDelay "amixer set Master 3%+")
       , ((mod4Mask, xK_Page_Down), spawnDelay "amixer set Master 3%-")
       , ((modShift, xK_End), spawnDelay "systemctl poweroff")
       , ((modShift, xK_Home), spawnDelay "systemctl reboot")
       , ((modShift, xK_Pause), nDebug "Hibernating..." >> spawnDelay "systemctl hibernate")
       , ((mod4Mask, xK_KP_Add), spawnDelay "ncmpcpp next")
       , ((mod4Mask, xK_KP_Subtract), spawnDelay "ncmpcpp prev")
       , ((mod4Mask, xK_KP_Enter), spawnDelay "ncmpcpp toggle")
       , ((mod4Mask, xK_KP_Insert), spawnDelay "notify-send \"`ncmpcpp --now-playing`\"")
       , ((modShift, xK_n), spawnDelay "urxvt -e 'bash -c \"connmanctl services ; connmanctl\"'")
       , ((modShift, xK_KP_Enter), spawnDelay "urxvt")
       , ((modShift, xK_numbersign), do
            nDebug "Recompiling XMonad"
            spawnDelay "xmonad --recompile && xmonad --restart && notify-send 'XMonad recompiled'")
       , ((modShift, xK_h), sendMessage ToggleStruts)

         -- brightness
       , ((mod4Mask, xF86XK_MonBrightnessUp), brightnessUp)
       , ((mod4Mask, xF86XK_MonBrightnessDown), brightnessDown)

       -- xmacro
       , ((modShift, xK_m), xmacroRecStart)
       , ((mod4Mask, xK_F8), xmacroRecStop)
       , ((mod4Mask, xK_F6), xmacroPlay)
       , ((mod4Mask, xK_F7), xmacroPlayFast)
       , ((mod4Mask, xF86XK_AudioRaiseVolume), raiseVolume alsa)
       , ((mod4Mask, xF86XK_AudioLowerVolume), lowerVolume alsa)
       , ((mod4Mask, xF86XK_AudioMute), muteVolume alsa)

       -- xtest
       , ((modShift, xK_z), toggleSpam spamToggle)
       , ((modShift, xK_l), nDebug $ workspaces defaultConfig !! 0)
       ]
    where
      modShift = mod4Mask .|. shiftMask
      alsa = "alsa_output.pci-0000_00_1b.0.analog-stereo"

_BRIGHTNESS_PATH :: String
_BRIGHTNESS_PATH = "/sys/class/backlight/gmux_backlight/brightness"

_BRIGHTNESS_MAX :: Int
_BRIGHTNESS_MAX = 512

_BRIGHTNESS_MIN :: Int
_BRIGHTNESS_MIN = 0

_BRIGHTNESS_DELTA :: Int
_BRIGHTNESS_DELTA = 32

getBrightness :: X (Maybe Int)
getBrightness = do
  brightnessRaw <- io $ withFile _BRIGHTNESS_PATH ReadMode hGetLine
  case reads brightnessRaw of
    [(b, [])] -> return (Just b)
    _ -> return Nothing

setBrightness :: Int -> X ()
setBrightness newBrightness = io $ do
  withFile _BRIGHTNESS_PATH WriteMode $ \handle -> do
    hPutStrLn handle (show newBrightness)

brightnessUp :: X ()
brightnessUp = modifyBrightness (\brightness -> brightness + _BRIGHTNESS_DELTA)

brightnessDown :: X ()
brightnessDown = modifyBrightness (\brightness -> brightness - _BRIGHTNESS_DELTA)

modifyBrightness :: (Int -> Int) -> X ()
modifyBrightness modifyFun = do
  mCurrentBrightness <- getBrightness
  case mCurrentBrightness of
    Nothing -> do
      return ()
    Just currentBrightness -> do
      setBrightness (max _BRIGHTNESS_MIN (min _BRIGHTNESS_MAX (modifyFun currentBrightness)))


type Sink = String

raiseVolume :: Sink -> X ()
raiseVolume sink = spawn $ "pactl set-sink-volume " <> sink <> " +5%"

lowerVolume :: Sink -> X ()
lowerVolume sink = spawn $ "pactl set-sink-volume " <> sink <> " -5%"

muteVolume :: Sink -> X ()
muteVolume sink = spawn $ "pactl set-sink-mute " <> sink <> " toggle"

toggleSpam :: MVar Bool -> X ()
toggleSpam mvar = unliftX $ \ul -> modifyMVar_ mvar $ \m ->
                  case m of
                    False -> ul (spawn "clickspammer") *> return True
                    True -> ul (spawn "killall clickspammer") *> return False

nDebug :: String -> X ()
nDebug str = spawnDelay $ "twmnc -c '" ++ str ++ "'"

xmacroRecStart :: X ()
xmacroRecStart = mapM_ spawn
                 [ "echo RECORDING > $HOME/.xmacro/status"
                 , "xmacrorec2 -k 74 > $HOME/.xmacro/macro" -- F8 for quit
                 ]
xmacroRecStop :: X ()
xmacroRecStop = spawn "echo > $HOME/.xmacro/status"

xmacroPlay :: X ()
xmacroPlay = do
    liftIO $ threadDelay 100000 -- needed so that keypress doesnt overlap with macro
    spawn "xmacroplay < $HOME/.xmacro/macro"

xmacroPlayFast :: X ()
xmacroPlayFast = do
    liftIO $ threadDelay 100000
    spawn "xmacroplay --speed 0 < $HOME/.xmacro/macro"


wAssignment :: Map.Map String WorkspaceId
wAssignment = Map.fromList [ ("chromium", "2")
                           , ("skype", "8")
                           , ("emacs", "1")
                           , ("urxvt -e ncmpcpp", "4")
                           ]

cmdLine :: ProcessID -> X String
cmdLine p = do
  let path = "/proc/" ++ show p ++ "/cmdline"
  comm <- io $ withFile path ReadMode hGetLine
  return . init . map (\c -> if c == '\NUL' then ' ' else c) $ comm

-- switch to assigned ws and start process if it's not there
wSwitch :: String -> X ()
wSwitch cmd = do
  flip (maybe $ return ()) (Map.lookup cmd wAssignment) $ \wid -> do
                windows (S.greedyView wid)
                wins <- S.allWindows <$> gets windowset
                winpids <- mapMaybe (\(w, mp) -> (w,) <$> mp) . zip wins <$> mapM (runQuery pid) wins
                mWindows <- forM winpids $ \(w, p) -> do
                  cmd2 <- cmdLine p
                  nDebug cmd2
                  return $ if (cmd2 == cmd)
                           then Just w
                           else Nothing
                case catMaybes mWindows of
                  (w : _) -> focus w
                  _ -> spawn cmd

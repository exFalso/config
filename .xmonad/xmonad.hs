{-# LANGUAGE TupleSections, RankNTypes #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, removeKeys)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as S

import Data.List
import System.IO
import Data.Monoid
import qualified Data.Map as Map
import Control.Concurrent
import Control.Applicative
import Control.Monad.Maybe
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Posix(ProcessID)
import Control.Monad.Writer
import Data.Conduit.Binary
import Data.Conduit
import qualified Data.Conduit.List as C
import qualified Data.ByteString.Char8 as BS

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


forkX :: X () -> X ThreadId
forkX x = unliftX $ \ul -> forkIO $ do
            nd <- openDisplay ""
            ul $ local (\r -> r  {display = nd}
                       ) x

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
  xmonad $ defaultConfig
       { manageHook = manageDocks <+> myManageHook
                      <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ spiral (1/2) ||| noBorders Full ||| Tall 1 (3/100) (1/2)
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
       , ((0, xK_Print), spawnDelay "scrot")
	   , ((modShift, xK_f), wSwitch "firefox") -- "conkeror &> $HOME/.conkeror.mozdev.org/log")
	   , ((modShift, xK_s), wSwitch "skype")
	   , ((modShift, xK_w), wSwitch "emacs")
	   , ((modShift, xK_r), wSwitch "urxvt -e ncmpcpp")
	   , ((modShift, xK_apostrophe), spawnDelay "shakemon")
       , ((mod4Mask, xK_p), spawnDelay "dmenu_run")
       , ((modShift, xK_g), spawnDelay "dmake $HOME/.gamesmake")
       , ((modShift, xK_e), spawnDelay "dmake $HOME/.editmake")
       , ((modShift, xK_d), spawnDelay "dmake $HOME/.displaymake")          
       , ((mod4Mask, xK_k), spawnDelay "dkill -15")          
       , ((modShift, xK_k), spawnDelay "dkill -9")          
       , ((modShift, xK_v), spawnDelay "gnome-volume-control")
       , ((mod4Mask, xK_Home), spawnDelay "amixer set Speaker 25 || amixer set Master 25")
       , ((mod4Mask, xK_End), spawnDelay "amixer set Speaker 0 || amixer set Master 0")
       , ((mod4Mask, xK_Page_Up), spawnDelay "amixer set Speaker 3%+ || amixer set Master 3%+")
       , ((mod4Mask, xK_Page_Down), spawnDelay "amixer set Speaker 3%- || amixer set Master 3%-")
       , ((modShift, xK_Page_Up), io $ changeBrightness (+ 1))
       , ((modShift, xK_Page_Down), io $ changeBrightness (\a -> a - 1))
       , ((modShift, xK_End), spawnDelay "systemctl poweroff")
       , ((modShift, xK_Home), spawnDelay "systemctl reboot")
       , ((modShift, xK_Pause), nDebug "Hibernating..." >> spawnDelay "systemctl hibernate")
       , ((mod4Mask, xK_KP_Add), spawnDelay "ncmpcpp next")
       , ((mod4Mask, xK_KP_Subtract), spawnDelay "ncmpcpp prev")
       , ((mod4Mask, xK_KP_Enter), spawnDelay "ncmpcpp toggle")
       , ((mod4Mask, xK_KP_Insert), spawnDelay "notify-send \"`ncmpcpp --now-playing`\"")
       , ((modShift, xK_n), spawnDelay "urxvt -e wicd-curses")
       , ((modShift, xK_KP_Enter), spawnDelay "urxvt")
       , ((modShift, xK_numbersign), do
            nDebug "Recompiling XMonad"
            spawnDelay "xmonad --recompile && xmonad --restart && notify-send 'XMonad recompiled'")
	   , ((modShift, xK_h), sendMessage ToggleStruts)
         
       -- xmacro
       , ((modShift, xK_m), xmacroRecStart)
       , ((mod4Mask, xK_F8), xmacroRecStop)
       , ((mod4Mask, xK_F6), xmacroPlay)
       , ((mod4Mask, xK_F7), xmacroPlayFast)
         
       -- xtest
       , ((modShift, xK_z), toggleSpam spamToggle)
       , ((modShift, xK_l), nDebug $ workspaces defaultConfig !! 0)
       ]
    where
      modShift = mod4Mask .|. shiftMask

changeBrightness :: (Int -> Int) -> IO ()
changeBrightness f = runResourceT $ do
  let f' = BS.pack . (show . f . read) . BS.unpack
  h <- lift $ openFile "/sys/class/backlight/acpi_video0/brightness" ReadWriteMode
  sourceHandle h $= C.map f' $$ sinkHandle h
  io $ hClose h

syncX :: X ()
syncX = withDisplay $ \d -> io $ sync d False

toggleSpam :: MVar Bool -> X ()
toggleSpam mvar = unliftX $ \ul -> modifyMVar_ mvar $ \m ->
                  case m of
                    False -> ul (spawn "clickspammer") *> return True
                    True -> ul (spawn "killall clickspammer") *> return False

nDebug :: String -> X ()
nDebug str = spawnDelay $ "notify-send '" ++ str ++ "'"

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
wAssignment = Map.fromList [ ("firefox", "2")
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
                window <- runMaybeT $ foldl (\m (w, p) ->
                                                 mplus m $ do
                                                   cmd2 <- lift $ cmdLine p
                                                   MaybeT . return $ if (cmd2 == cmd)
                                                                     then Just w
                                                                     else Nothing
                                            ) mzero winpids
                maybe (spawn cmd) focus window

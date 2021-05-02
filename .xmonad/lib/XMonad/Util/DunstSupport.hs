
module XMonad.Util.DunstSupport (
  dunstLogHook
) where

import qualified Data.List as L
import Foreign.C.Types
import Graphics.X11.Xlib

import XMonad

------------------------------------------------------------------------------------------
-- Support for notification (_NET_WM_STATE = _NET_WM_STATE_ABOVE) to be always on top
------------------------------------------------------------------------------------------
dunstLogHook = aboveStateWindowsOnTop

aboveStateWindowsOnTop = do
    rw <- asks theRoot
    dpy <- asks display
    (_, _, windows) <- io $ queryTree dpy rw
    windowsWithWmState <- windowsWithWmState dpy windows
    atom_NET_WM_STATE_ABOVE <- getAtom "_NET_WM_STATE_ABOVE"
    let aboveStateWindows = L.filter (L.elem atom_NET_WM_STATE_ABOVE . snd) windowsWithWmState
--    let aboveWindows = L.filter (L.elem  . snd) windowsWithWmState
--    io $ appendFile "/tmp/xmonad.debug.dunst" $ (show aboveStateWindows) ++ "\n"
    io $ L.foldr (>>) (return ()) $ L.map (raiseWindow dpy . fst) aboveStateWindows

windowsWithWmState dpy ws = do
  atom <- getAtom "_NET_WM_STATE"
  windowsWithWmState' atom dpy ws

windowsWithWmState' atom dpy (w:ws) = do
  mp <- io $ getWindowProperty32 dpy atom w
  r <- windowsWithWmState' atom dpy ws
  io $ case mp of
    Just p -> do
      return $ (w, L.map (\(CLong n) -> fromIntegral n :: Atom) p):r
--            name <- getAtomNames dpy $ L.map (\(CLong n) -> fromIntegral n) p
--            return $ (w, name):r
    Nothing -> return r
windowsWithWmState' atom dpy [] = return []
------------------------------------------------------------------------------------------
-- Support for notification (_NET_WM_STATE = _NET_WM_STATE_ABOVE) to be always on top
------------------------------------------------------------------------------------------

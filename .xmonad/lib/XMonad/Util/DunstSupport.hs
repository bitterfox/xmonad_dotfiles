
module XMonad.Util.DunstSupport (
  dunstEventHook
) where

import Data.Monoid
import qualified Data.List as L
import Foreign.C.Types
import Graphics.X11.Xlib

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

------------------------------------------------------------------------------------------
-- Support for notification (_NET_WM_STATE = _NET_WM_STATE_ABOVE) to be always on top
------------------------------------------------------------------------------------------
data AboveStateWindows = AboveStateWindows [Window] deriving Typeable
instance ExtensionClass AboveStateWindows where
  initialValue = AboveStateWindows []

dunstEventHook = aboveStateWindowsOnTop
aboveStateWindowsOnTop e = do
  case e of
    (MapNotifyEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_override_redirect) -> do
      AboveStateWindows windows <- XS.get
      if L.notElem ev_window windows then do
        atom_NET_WM_STATE_ABOVE <- getAtom "_NET_WM_STATE_ABOVE"
        wmstates <- getWmState ev_event_display ev_window
        if L.elem atom_NET_WM_STATE_ABOVE wmstates then do
          XS.put $ AboveStateWindows $ ev_window:windows
--          spawn $ "echo 'Map ABOVE " ++ (show ev_window) ++ ", " ++ (show windows) ++ "' >> /tmp/xmonad.debug.dunst"
          return (All True)
        else do
          io $ L.foldr (>>) (return ()) $ L.map (raiseWindow ev_event_display) windows
--          spawn $ "echo 'Map " ++ (show ev_window) ++ ", " ++ (show windows) ++ "' >> /tmp/xmonad.debug.dunst"
          return (All True)
      else return (All True)
    (UnmapEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_from_configure) -> do
      AboveStateWindows windows <- XS.get
      if L.elem ev_window windows then do
        XS.put $ AboveStateWindows $ L.delete ev_window windows
--      spawn $ "echo 'Unmap ABOVE " ++ (show ev_window) ++ ", " ++ (show ev_parent) ++ "' >> /tmp/xmonad.debug.dunst"
        return (All True)
      else return (All True)
    _ -> return (All True)

getWmState dpy w = do
  atom <- getAtom "_NET_WM_STATE"
  mp <- io $ getWindowProperty32 dpy atom w
  io $ case mp of
    Just p -> do
      return $ L.map (\(CLong n) -> fromIntegral n :: Atom) p
    Nothing -> return []
------------------------------------------------------------------------------------------
-- Support for notification (_NET_WM_STATE = _NET_WM_STATE_ABOVE) to be always on top
------------------------------------------------------------------------------------------

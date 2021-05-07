
module XMonad.Util.HandleEventHooks (
  fullScreenEventHook,
  keepWindowSizeHandleEventHook
) where

import qualified Data.List as L
import Data.Monoid
import qualified Data.Map.Strict as M
import Foreign
import Foreign.C.Types

import Graphics.X11.Xlib

import XMonad
import qualified XMonad.StackSet as W

fullScreenEventHook e@(ClientMessageEvent {ev_message_type = mt, ev_data = d, ev_window = w}) = withDisplay $ \dpy -> do
  whenX (((mt ==) <$> getAtom "_NET_WM_STATE") <&&> ((\atom -> L.any (atom ==) $ L.map fromIntegral d) <$> getAtom "_NET_WM_STATE_FULLSCREEN")) $
    withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
      setEventType ev configureNotify
      setConfigureEvent ev w w
        (wa_x wa) (wa_y wa) (wa_width wa)
        (wa_height wa) (wa_border_width wa) none (wa_override_redirect wa)
      sendEvent dpy w False 0 ev
  return (All True)
fullScreenEventHook _ = return (All True)


keepWindowSizeHandleEventHook query e@(ConfigureRequestEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_x ev_y ev_width ev_height ev_border_width ev_above ev_detail ev_value_mask) = withDisplay $ \dpy -> do
  ws <- gets windowset
  bw <- asks (borderWidth . config)
  isTarget <- runQuery query ev_window

  if (M.member ev_window (W.floating ws)) && (W.member ev_window ws) && isTarget && (testBit ev_value_mask 0 || testBit ev_value_mask 1 || testBit ev_value_mask 2 || testBit ev_value_mask 3) then do
    let w = ev_window
    withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev w w
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width) none (wa_override_redirect wa)
                 sendEvent dpy w False 0 ev
    io $ sync dpy False
    return (All False)
  else return (All True)
keepWindowSizeHandleEventHook _ _ = return (All True)

------------------------------------------------------------------------------------------
-- XLib
------------------------------------------------------------------------------------------
setConfigureRequestEvent ev (ConfigureRequestEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_x ev_y ev_width ev_height ev_border_width ev_above ev_detail ev_value_mask) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 32) ev ev_parent
    (\hsc_ptr -> pokeByteOff hsc_ptr 40) ev ev_window
    (\hsc_ptr -> pokeByteOff hsc_ptr 48) ev ev_x
    (\hsc_ptr -> pokeByteOff hsc_ptr 52) ev ev_y
    (\hsc_ptr -> pokeByteOff hsc_ptr 56) ev ev_width
    (\hsc_ptr -> pokeByteOff hsc_ptr 60) ev ev_height
    (\hsc_ptr -> pokeByteOff hsc_ptr 64) ev ev_border_width
    (\hsc_ptr -> pokeByteOff hsc_ptr 72) ev ev_above
    (\hsc_ptr -> pokeByteOff hsc_ptr 80) ev ev_detail
    (\hsc_ptr -> pokeByteOff hsc_ptr 88) ev ev_value_mask
------------------------------------------------------------------------------------------
-- XLib
------------------------------------------------------------------------------------------

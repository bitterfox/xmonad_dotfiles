
module XMonad.Util.HandleEventHooks (
  fullScreenEventHook
) where

import Data.Monoid
import XMonad

fullScreenEventHook e@(ClientMessageEvent {ev_message_type = mt, ev_data = d, ev_window = w}) = withDisplay $ \dpy -> do
  whenX ((mt ==) <$> getAtom "_NET_WM_STATE") $
    whenX (((fromIntegral $ d!!1) ==) <$> getAtom "_NET_WM_STATE_FULLSCREEN") $ do
      spawn $ "echo 'configureNotify' >> /tmp/xmonad.debug.event"
      withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
        setEventType ev configureNotify
        setConfigureEvent ev w w
          (wa_x wa) (wa_y wa) (wa_width wa)
          (wa_height wa) (wa_border_width wa) none (wa_override_redirect wa)
        sendEvent dpy w False 0 ev
  return (All True)
fullScreenEventHook _ = return (All True)

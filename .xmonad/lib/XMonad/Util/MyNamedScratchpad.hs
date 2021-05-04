
module XMonad.Util.MyNamedScratchpad (
  terminalScratchpad,
  namedScratchpadHandleEventHook,
  keepWindowSizeHandleEventHook,
  myNamedScratchpadActionInternal,
  toggleScrachpadAction,
  runScratchpadAction
) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Monoid
import Foreign
import Foreign.C.Types

import Graphics.X11.Xlib

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.ExtensibleState as XS

-- TODO Isolate setConfigureRequestEvent and keepWindowSizeHandleEventHook to other module
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

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------

terminalScratchpad :: String -> Maybe String -> ManageHook -> NamedScratchpad
terminalScratchpad name execMaybe manageHook =
    NS name
       ("/usr/lib/gnome-terminal/gnome-terminal-server" ++
           " --app-id bitter_fox.xmonad." ++ name ++
           " --name=" ++ name ++ " --class=" ++ name ++
           " & gnome-terminal --app-id bitter_fox.xmonad." ++ name ++
           (case execMaybe of
              Just exec -> " -e " ++ exec
              Nothing -> ""
           )
       )
       (appName =? name)
       manageHook

data NamedScratchpadSendEventWindows = NamedScratchpadSendEventWindows [Window] deriving Typeable
instance ExtensionClass NamedScratchpadSendEventWindows where
  initialValue = NamedScratchpadSendEventWindows []

namedScratchpadHandleEventHook scratchpads = keepWindowSizeHandleEventHook $ L.foldr (<||>) (return False) $ L.map query scratchpads
keepWindowSizeHandleEventHook query e@(ConfigureRequestEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_x ev_y ev_width ev_height ev_border_width ev_above ev_detail ev_value_mask) = withDisplay $ \dpy -> do
  ws <- gets windowset
  bw <- asks (borderWidth . config)
  isTarget <- runQuery query ev_window
  spawn $ "echo '"++ (show isTarget) ++"' >> /tmp/xmonad.debug.event"

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

myNamedScratchpadActionInternal scratchpads n = do
    namedScratchpadAction scratchpads n
    myNamedScratchpadRelocationAction scratchpads n

myNamedScratchpadRelocationAction scratchpads n = do
      withWindowSet (\s ->
         case (W.peek s, L.find isSameName scratchpads) of
           (Just w, Just ns) -> do
             hook <- runQuery (hook ns) w
             isTarget <- runQuery (query ns) w
             case isTarget of
               True -> windows(\ws -> appEndo hook ws)
               _ -> return ()
           _ -> return ()
       )
  where
    isSameName = \ns -> n == (name ns)

runScratchpadAction scratchpad = myNamedScratchpadActionInternal [scratchpad] $ name scratchpad

toggleScrachpadAction scratchpads =
    withWindowSet (\s ->
      case W.peek s of
        Just w -> do
          (cur, next) <- findCurrentAndNextScrachpadOf scratchpads w
          myNamedScratchpadActionMaybe cur >> myNamedScratchpadActionMaybe next
        _ -> doAction $ name (head scratchpads)
    )
  where doAction = myNamedScratchpadActionInternal scratchpads
        myNamedScratchpadActionMaybe mns =
            whenJust mns $ \ns -> doAction $ name ns

findCurrentAndNextScrachpadOf nss w =
    findIt (cycle ((asMaybe nss) ++ [Nothing]))
  where
    asMaybe = \l -> map Just l
    findIt = \l ->
               let
                   h = head l
                   t = tail l
               in
                 case h of
                   Just ns -> do
                          matched <- runQuery (query ns) w
                          if matched then
                              return (h, head t)
                          else
                              findIt t
                   _ -> return (h, head t)

isScratchpadWindow (s:ss) w = do
  isTarget <- runQuery (query s) w
  case isTarget of
    True -> return True
    False -> isScratchpadWindow ss w

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------

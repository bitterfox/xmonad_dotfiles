module XMonad.Util.AdvancedMouse(
  whenDoubleClick,
  advancedMouseBindings,
  ungrabButtons',
  advancedMouseEventHook,
) where

import Data.Bits
import Data.Time.Clock
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid

import Foreign.C.Types

import Graphics.X11.Types

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

-- AdvancedMouseState <target buttons> <current pressed button (mask)> <button press histories>
data AdvancedMouseState = AdvancedMouseState [Button] ButtonMask [(Button, UTCTime)] deriving (Typeable)
instance ExtensionClass AdvancedMouseState where
  initialValue = AdvancedMouseState [] 0 []

buttonMask :: Button -> ButtonMask
buttonMask id = (1 :: CUInt) `shift` (fromIntegral (7 + id))

advancedMouseBindings :: XConfig a -> [([Button], (KeyMask, Button), (Window -> X ()))] -> XConfig a
advancedMouseBindings conf list = do
  let conf' = L.foldr (\(bs, (m, b), a) -> \c -> advancedMouseBinding c bs m b a) conf list
  let targetButtons = list >>= (\(bs, _, _) -> bs)
  conf' {startupHook = do
           startupHook conf'
           AdvancedMouseState oldTargetButtons mask history <- XS.get
           XS.put $ AdvancedMouseState (L.nub $ oldTargetButtons ++ targetButtons) mask history
        }
infixl 4 `advancedMouseBindings`

advancedMouseBinding :: XConfig a -> [Button] -> KeyMask -> Button -> (Window -> X ()) -> XConfig a
advancedMouseBinding conf buttons mask button action = do
    let conf' = L.foldr (\b -> \c -> rebindMouseBinding (\a -> \w -> do
                                          onlyTargetButtonPressed <- testButtonMask $ buttonMask b
                                          if onlyTargetButtonPressed then a w else return ()) c 0 b) conf buttons
    let bm = L.foldr (\l -> \r -> (buttonMask l) .|. r) 0 buttons
    let conf'' = rebindMouseBinding (\a -> \w -> do
                                             b <- testButtonMask bm
                                             (if b then action else a) w
                                    ) conf' mask button
    conf''

rebindMouseBinding f conf mask button =
  conf {mouseBindings = \xconfig -> do
                          let actions = mouseBindings conf xconfig
                          let action = fromMaybe (\w -> return ()) $ M.lookup (mask, button) $ actions
                          let newAction = f action
                          M.insert (mask, button) newAction actions
       }

isButtonPressed :: X Bool
isButtonPressed = do
  AdvancedMouseState _ mask last <- XS.get
  spawn $ "echo 'test isButtonPressed' >> /tmp/xmonad.debug.event"
  return $ mask /= 0

testButtonMask :: ButtonMask -> X Bool
testButtonMask mask = do
  AdvancedMouseState _ m _ <- XS.get
  spawn $ "echo 'test "++(show m) ++ " with "++(show m)++"' >> /tmp/xmonad.debug.event"
  return $ mask == m

advancedMouseEventHook e@ButtonEvent{ev_event_type = ev_event_type, ev_button = ev_button} = do
  AdvancedMouseState buttons mask history <- XS.get
  if L.any (ev_button ==) buttons then do
    let newMask = if ev_event_type == buttonPress then
                     mask .|. (buttonMask ev_button)
                 else
                     mask .&. (complement (buttonMask ev_button))
    if ev_event_type == buttonPress then
        grabPointer'
    else if newMask == 0 then
        ungrabPointer'
    else return ()
    newLastTime <- io $ getCurrentTime
    let newHistory = if ev_event_type == buttonPress then
                     (ev_button, newLastTime):history
                 else history
    XS.put $ AdvancedMouseState buttons newMask (limitSize 5 $ newHistory)
    spawn $ "echo 'Before: " ++ (show mask) ++ ", " ++ (show newHistory) ++ "' >> /tmp/xmonad.debug.event"
    spawn $ "echo 'After: " ++ (show newMask) ++ ", " ++ (show (ev_button, newLastTime)) ++ "' >> /tmp/xmonad.debug.event"
  else do
    spawn $ "echo 'Ignore: " ++ (show e) ++ "' >> /tmp/xmonad.debug.event"
    return ()
  return $ All True
advancedMouseEventHook e = do
  return $ All True

grabButtons' = do
    XConf { display = dpy, theRoot = rootw } <- ask
    let grab button mask = io $ grabButton dpy button mask rootw False (buttonPressMask .|. buttonReleaseMask)
                                           grabModeAsync grabModeSync none none
    io $ ungrabButton dpy anyButton anyModifier rootw
    ems <- extraModifiers
    ba <- asks buttonActions
    mapM_ (\(m,b) -> mapM_ (grab b . (m .|.)) ems) (M.keys $ ba)
ungrabButtons' conf (x:xs) = do
  let conf' = conf { startupHook = do
                       startupHook conf
                       XConf { display = dpy, theRoot = rootw } <- ask
                       io $ ungrabButton dpy x 0 rootw
                       -- io $ ungrabButton dpy x anyModifier rootw
                   }
  ungrabButtons' conf' xs
ungrabButtons' conf [] = conf
infixl 4 `ungrabButtons'`

grabPointer' = do
  XConf { theRoot = root, display = d } <- ask
  io $ grabPointer d root False (buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask)
         grabModeAsync grabModeAsync none none currentTime
  return ()

ungrabPointer' = withDisplay $ io . flip ungrabPointer currentTime

whenDoubleClick interval buttonFirst buttonSecond x = do
  dc <- isDoubleClick (0.300 :: NominalDiffTime) buttonFirst buttonSecond
  if dc then x
  else return ()

isDoubleClick interval buttonFirst buttonSecond = do
  AdvancedMouseState _ mask history <- XS.get
  if L.length history >= 2 then do
      let (buttonFirst', timeFirst) = head history
      let (buttonSecond', timeSecond) = head $ tail history
      return $ buttonFirst == buttonFirst' && buttonSecond == buttonSecond' && diffUTCTime timeFirst timeSecond <= interval
  else
      return False

clearHistory :: X ()
clearHistory = do
  AdvancedMouseState buttons mask history <- XS.get
  XS.put $ AdvancedMouseState buttons mask []

limitSize x list = limitSize' x [] list
limitSize' x l1 l2 =
    if x > 0 && (L.length l2) > 0 then limitSize' (x-1) ((head l2):l1) $ tail l2
    else L.reverse l1

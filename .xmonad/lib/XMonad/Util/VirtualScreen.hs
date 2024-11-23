{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module XMonad.Util.VirtualScreen (
      VirtualScreen(rootSid, originalRect, screenLayout, screenStack),
      VirtualScreens,
      virtualScreenLogHook,
      getVirtualScreens,
      resetVirtualScreens,
      resetVirtualScreen,
      findVirtualScreen,
      currentVirtualScreen,
      getAllVirtualScreenIds,
      createVirtualScreen,
      sendScreenMessage,
      rootSids,
      nextRootScreen, prevRootScreen,
      nextVirtualScreen, prevVirtualScreen,
      shiftNextRootScreen, shiftPrevRootScreen,
      nextChildScreen, prevChildScreen
) where

import qualified Data.List as L
import Data.Maybe

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

------------------------------------------------------------------------------------------
-- Virtual screen a.k.a. Split screen
------------------------------------------------------------------------------------------
data ScreenLayout a = forall l. (LayoutClass l a, Read (l a)) => ScreenLayout (l a)

instance LayoutClass ScreenLayout ScreenId where
    runLayout (W.Workspace i (ScreenLayout l) ms) r = fmap (fmap ScreenLayout) `fmap` runLayout (W.Workspace i l ms) r
    doLayout (ScreenLayout l) r s  = fmap (fmap ScreenLayout) `fmap` doLayout l r s
    emptyLayout (ScreenLayout l) r = fmap (fmap ScreenLayout) `fmap` emptyLayout l r
    handleMessage (ScreenLayout l) = fmap (fmap ScreenLayout) . handleMessage l
    description (ScreenLayout l)   = description l

instance Show (ScreenLayout a) where show (ScreenLayout l) = show l

data VirtualScreen = VirtualScreen {
  rootSid :: ScreenId,
  originalRect :: Rectangle,
  screenLayout :: ScreenLayout ScreenId,
  screenStack :: W.Stack ScreenId
} deriving (Typeable, Show)

data VirtualScreens = VirtualScreens [VirtualScreen] deriving (Typeable, Show)
instance ExtensionClass VirtualScreens where
  initialValue = VirtualScreens []

emptyVirtualScreens = VirtualScreens []

virtualScreenLogHook = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let sid = W.screen $ W.current ws
    whenJust (findVirtualScreen virtualScreens sid) $ \vs ->
      whenJust (focusIt sid $ screenStack vs) $ \s ->
        XS.put $ replaceVirtualScreen virtualScreens $ vs {screenStack = s}

getVirtualScreens :: X VirtualScreens
getVirtualScreens = XS.get

resetVirtualScreens :: X ()
resetVirtualScreens = XS.put $ emptyVirtualScreens


findVirtualScreen :: VirtualScreens -> ScreenId -> Maybe VirtualScreen
findVirtualScreen (VirtualScreens virtualScreens) sid =
  L.find (L.elem sid . W.integrate . screenStack) virtualScreens

currentVirtualScreen :: X (Maybe VirtualScreen)
currentVirtualScreen = do
  vss <- getVirtualScreens
  sid <- withWindowSet $ return . W.screen . W.current
  return $ findVirtualScreen vss sid

getAllVirtualScreenIds :: VirtualScreen -> [ScreenId]
getAllVirtualScreenIds vs =
    L.filter (rootSid vs /=) $ W.integrate $ screenStack vs

newVirtualScreen screen layout =
  VirtualScreen {
    rootSid = sid,
    originalRect = screenRect $ W.screenDetail screen,
    screenLayout = ScreenLayout layout,
    screenStack = W.Stack {
                    W.focus = sid,
                    W.up = [],
                    W.down = []
                  }
  }
  where sid = W.screen screen

insertScreenStack :: VirtualScreen -> ScreenId -> VirtualScreen
insertScreenStack vs@VirtualScreen{screenStack = ss} sid =
  vs {
    screenStack = ss {
                    W.down = down ++ [sid]
                  }
  }
  where down = W.down ss

replaceVirtualScreen :: VirtualScreens -> VirtualScreen -> VirtualScreens
replaceVirtualScreen (VirtualScreens vss) vs =
  VirtualScreens $ vs:(L.filter ((rootSid vs /=) . rootSid) vss)

resetVirtualScreen :: X()
resetVirtualScreen = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let current = W.current ws
    let sid = W.screen current
    case findVirtualScreen virtualScreens $ sid of
      Just vs -> do
        let sids = L.delete sid $ W.integrate $ screenStack vs
        let newVisible = L.filter (\e -> L.notElem (W.screen e) sids) $ W.visible ws
        let workspaces = L.map W.workspace $ L.filter (\e -> L.elem (W.screen e) sids) $ W.visible ws
        XS.put $ replaceVirtualScreen virtualScreens $ vs {
                                                         screenStack = W.Stack { W.focus = rootSid vs, W.up = [], W.down = [] }
                                                       }
        windows $ \_ -> ws {
          W.current = current {
                        W.screen = rootSid vs,
                        W.screenDetail = SD $ originalRect vs
                      },
          W.visible = newVisible,
          W.hidden = (W.hidden ws) ++ workspaces
        }
      Nothing -> return ()


createVirtualScreen :: (LayoutClass l ScreenId, Read (l ScreenId)) => (l ScreenId) -> X ()
createVirtualScreen defaultLayout = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> whenX (return $ not $ L.null $ W.hidden ws) $ do
    let current = W.current ws
    let visible = W.visible ws
    let allScreen = current:visible
    let vs = case findVirtualScreen virtualScreens $ W.screen current of
               Just vs -> vs
               Nothing -> newVirtualScreen current defaultLayout

    let newSid = S $ (L.maximum $ L.map (\(S id) -> id) $ L.map (W.screen) allScreen) + 1
    let nvs = insertScreenStack vs newSid

    (newRects, newLayoutMaybe) <- runLayout (W.Workspace {
                 W.tag = show $ rootSid nvs,
                 W.layout = screenLayout nvs,
                 W.stack = Just $ screenStack nvs}) (originalRect nvs)

    XS.put $ replaceVirtualScreen virtualScreens $ case newLayoutMaybe of
                                                     Just newLayout -> nvs {screenLayout = newLayout}
                                                     Nothing -> nvs

    let nextWS = head $ W.hidden ws
    let newHidden = tail $ W.hidden ws

    let newCurrent = replaceScreenRect newRects $ W.Screen {
                       W.workspace = nextWS,
                       W.screen = newSid,
                       W.screenDetail = SD $ originalRect nvs
                     }
    let newVisible = L.map (replaceScreenRect newRects) allScreen

    windows $ \_ -> ws {
      W.current = newCurrent,
      W.visible = newVisible,
      W.hidden = newHidden
    }

replaceScreenRect rects screen =
  case findScreenRect rects $ W.screen screen of
    Just rect -> screen {W.screenDetail = SD rect}
    Nothing -> screen

findScreenRect :: [(ScreenId, Rectangle)] -> ScreenId -> Maybe Rectangle
findScreenRect rects sid = fmap snd $ L.find ((sid ==) . fst) rects

sendScreenMessage msg = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let current = W.current ws
    spawn $ "echo '" ++ (show virtualScreens) ++ "' >> /tmp/xmonad.debug.screen"
    case findVirtualScreen virtualScreens $ W.screen current of
      Just vs -> do
        let stack = screenStack vs
        let (up, focus:down) = span (W.screen current /=) $ W.integrate stack
        let newStack = W.Stack {W.focus = focus, W.up = L.reverse up, W.down = down}
        (_, layoutMaybe) <- runLayout (W.Workspace {
                 W.tag = show $ rootSid vs,
                 W.layout = screenLayout vs,
                 W.stack = Just $ newStack}) (originalRect vs)
        spawn $ "echo '" ++ (show layoutMaybe) ++ "' >> /tmp/xmonad.debug.screen"
        let layout = case layoutMaybe of
                       Just l -> l
                       Nothing -> screenLayout vs
        newLayoutMaybe <- handleMessage layout (SomeMessage msg) `catchX` return Nothing
        whenJust newLayoutMaybe $ \l -> do
          (rect, _) <- runLayout (W.Workspace {
                 W.tag = show $ rootSid vs,
                 W.layout = l,
                 W.stack = Just $ newStack}) (originalRect vs)
          XS.put $ replaceVirtualScreen virtualScreens $ vs {screenLayout = l}
          let visible = W.visible ws
          windows $ \_ -> ws {
            W.current = replaceScreenRect rect current,
            W.visible = L.map (replaceScreenRect rect) visible
          }
      Nothing -> return ()

rootSids vss ws = L.sort $ L.nub $ L.map (findRootSid vss) $ W.screens ws

currentRootSid vss ws = findRootSid vss $ W.current ws

findRootSid vss screen = case findVirtualScreen vss sid of
                        Just vs -> rootSid vs
                        Nothing -> sid
  where sid = W.screen screen

nextRootScreen = focusRootScreen id
prevRootScreen = focusRootScreen L.reverse
focusRootScreen f = do
  virtualScreens <- XS.get
  windows $ \ws -> do
    let rootSids' = rootSids virtualScreens ws
    let currentRoot = currentRootSid virtualScreens ws
    let nextSid = head $ tail $ dropWhile (currentRoot /=) $ cycle $ f rootSids'
    let (up, focus:down) = L.span ((nextSid /=) . W.screen) $ W.screens ws
    ws {
      W.current = focus,
      W.visible = up ++ down
    }

nextVirtualScreen = focusVirtualScreen id
prevVirtualScreen = focusVirtualScreen L.reverse
focusVirtualScreen f = do
  virtualScreens <- XS.get
  withWindowSet $ \ws -> do
    let sid = W.screen $ W.current ws
    whenJust (findVirtualScreen virtualScreens sid) $ \vs ->
      whenJust (focusIt sid $ screenStack vs) $ \s ->
        XS.put $ replaceVirtualScreen virtualScreens $ vs {screenStack = s}
    windows $ \ws -> do
      let rootSids' = rootSids virtualScreens ws
      let currentRoot = currentRootSid virtualScreens ws
      let nextRootSid = head $ tail $ dropWhile (currentRoot /=) $ cycle $ f rootSids'
      let focusSid = fromMaybe nextRootSid $ W.focus <$> screenStack <$> findVirtualScreen virtualScreens nextRootSid
      let (up, focus:down) = L.span ((focusSid /=) . W.screen) $ W.screens ws
      ws {
        W.current = focus,
        W.visible = up ++ down
      }

shiftNextRootScreen = shiftRootScreen id
shiftPrevRootScreen = shiftRootScreen L.reverse
shiftRootScreen f = do
  virtualScreens <- XS.get
  windows $ \ws -> do
    let rootSids' = rootSids virtualScreens ws
    let currentRoot = currentRootSid virtualScreens ws
    let prevSid = head $ tail $ dropWhile (currentRoot /=) $ cycle $ f rootSids'
    case L.find ((prevSid ==) . W.screen) $ W.screens ws of
      Just s -> W.shift (W.tag $ W.workspace s) ws
      Nothing -> ws

focusIt a stack = do
  let (up, focusDown) = L.span (a /=) $ W.integrate stack
  case focusDown of
    f:d -> Just $ W.Stack {W.focus = f, W.up = L.reverse up, W.down = d}
    _ -> Nothing
nextChildScreen = focusChildScreen W.focusDown'
prevChildScreen = focusChildScreen W.focusUp'
focusChildScreen f = do
  virtualScreens <- XS.get
  windows $ \ws -> do
    let currentSid = W.screen $ W.current ws
    case findVirtualScreen virtualScreens currentSid of
      Just vs -> do
        case focusIt currentSid $ screenStack vs of
          Just stack -> do
            let newStack = f stack
            let focus = W.focus newStack
            if focus == currentSid then ws
            else case L.find ((focus ==) . W.screen) $ W.visible ws of
                   Just s -> ws {
                               W.current = s,
                               W.visible = (W.current ws):(L.filter ((focus /=) . W.screen) $ W.visible ws)
                             }
                   Nothing -> ws
          _ -> ws
      Nothing -> ws

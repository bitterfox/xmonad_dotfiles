
module XMonad.Util.PhysicalScreen (
  EDID,
  rePhysicalScreen,
  originalScreenIdToCurrentScreenId,
  OriginalDisplayIdToCurrentScreenId(..),
) where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import XMonad
import qualified XMonad.StackSet as W

import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run(runProcessWithInput)

screenInfo screenDetail = (show $ rect_width $ screenDetail) ++ "x" ++ (show $ rect_height $ screenDetail) ++ "+" ++ (show $ rect_x $ screenDetail) ++ "+" ++ (show $ rect_y $ screenDetail)

getEDID :: Rectangle -> X EDID
getEDID screenDetail = do
  let si = screenInfo screenDetail
  edid <- runProcessWithInput "sh" ["-c", "xrandr --verbose | grep -A1000 ' connected .*" ++ si ++ "' | grep -A1 EDID | head -n 2 | tail -n 1 | awk '{print $1}' | xargs echo -n"] ""
  return ((if null edid then si else edid) :: EDID)

debugEDID = getCurrentScreenEDIDMap

getCurrentScreenEDIDMap = withWindowSet $ \s -> do
                            screenEDIDList <- toScreenEDIDList ((W.current s) : (W.visible s))
                            io $ appendFile "/tmp/debug" $ (show screenEDIDList)

toScreenEDIDList [] = return []
toScreenEDIDList (screen:rest) = do
    let screenId = W.screen screen
    edid <- getEDID $ screenRect $ W.screenDetail $ screen
    screenEDIDList <- toScreenEDIDList rest
    return $ (screenId, edid) : screenEDIDList

type EDID = String
data ScreenEDIDMap = ScreenEDIDMap (M.Map ScreenId EDID) deriving Typeable
instance ExtensionClass ScreenEDIDMap where
  initialValue = ScreenEDIDMap M.empty

rePhysicalScreen :: [EDID] -> X ()
rePhysicalScreen priorityDisplayEDIDs = do
    xinesc <- (withDisplay getCleanedScreenInfo) :: X [Rectangle]
    spawn $ "echo 'xinesc: " ++ (show xinesc) ++ "' >> /tmp/xmonad.debug"

    edidToScreenRectangles <- (mapM (\screenRectangle -> do
        edid <- getEDID screenRectangle
        return (edid, screenRectangle)) xinesc) :: X [(EDID, Rectangle)]
    spawn $ "echo 'edidToScreenRectangles: " ++ (show edidToScreenRectangles) ++ "' >> /tmp/xmonad.debug"
    let edidToScreenRectanglesMap = M.fromList edidToScreenRectangles
    spawn $ "echo 'edidToScreenRectanglesMap: " ++ (show edidToScreenRectanglesMap) ++ "' >> /tmp/xmonad.debug"

    let prioritiedEDIDToScreenRectangles = L.foldr (++) [] $ L.map (\edid -> case M.lookup edid edidToScreenRectanglesMap of
                                                                       Just screenRectangle -> [(edid, screenRectangle)]
                                                                       Nothing -> []) priorityDisplayEDIDs
    spawn $ "echo 'prioritiedEDIDToScreenRectangles" ++ (show prioritiedEDIDToScreenRectangles) ++ "' >> /tmp/xmonad.debug"
    let sortedEDIDToScreenRectangles = prioritiedEDIDToScreenRectangles ++ (L.filter (\(edid, screenRectangle) -> not $ edid `elem` priorityDisplayEDIDs) edidToScreenRectangles)

    originalToCurrent <- originalScreenIdToCurrentScreenId priorityDisplayEDIDs
    spawn $ "echo 'originalToCurrent" ++ (show originalToCurrent) ++ "' >> /tmp/xmonad.debug"
    XS.put $ OriginalDisplayIdToCurrentScreenId $ originalToCurrent

    spawn $ "echo '" ++ (show sortedEDIDToScreenRectangles) ++ "' >> /tmp/xmonad.debug"
    windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
        let (xs, ys) = splitAt (length xinesc) $ map W.workspace (L.sortOn (W.screen) (v:vs)) ++ hs
            (a:as)   = zipWith3 W.Screen xs [0..] $ map SD $ map (snd) sortedEDIDToScreenRectangles
        in  ws { W.current = a
               , W.visible = as
               , W.hidden  = ys }

data OriginalDisplayIdToCurrentScreenId = OriginalDisplayIdToCurrentScreenId (M.Map Int Int) deriving Typeable
instance ExtensionClass OriginalDisplayIdToCurrentScreenId where
  initialValue = OriginalDisplayIdToCurrentScreenId M.empty

originalScreenIdToCurrentScreenId priorityDisplayEDIDs = do
    xinesc <- (withDisplay getCleanedScreenInfo) :: X [Rectangle]

    edidToOriginalScreenIds <- (mapM (\(i, screenRectangle) -> do
        edid <- getEDID screenRectangle
        return (edid, i)) $ indexed xinesc) :: X [(EDID, Int)]

    let edidToOriginalScreenIdsMap = M.fromList edidToOriginalScreenIds

    let prioritiedOriginalScreenIds = L.foldr (++) [] $ L.map (\edid -> case M.lookup edid edidToOriginalScreenIdsMap of
                                                                       Just i -> [i]
                                                                       Nothing -> []) priorityDisplayEDIDs
    let originalScreenIds = prioritiedOriginalScreenIds ++ (L.map (snd) $ L.filter (\(edid, i) -> not $ edid `elem` priorityDisplayEDIDs) edidToOriginalScreenIds)

    return $ M.fromList $ L.map (\(currentScreenId, originalScreenId) -> (originalScreenId, currentScreenId) ) $ indexed originalScreenIds


--sortedEDIDToScreenRectangles (priorityDisplayEDID:rest) edidToScreenRectanglesMap =
--    case M.lookup edid

indexed l = L.zip [0..(L.length l)] l

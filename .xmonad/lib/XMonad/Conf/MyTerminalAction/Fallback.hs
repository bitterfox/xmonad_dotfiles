module XMonad.Conf.MyTerminalAction.Fallback where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.GridSelect

import XMonad.Util.WorkspaceFamily
import XMonad.Util.NamedScratchpad2
import XMonad.Util.MyNamedScratchpad
import XMonad.Util.MyUtils

import XMonad.Conf.MyConf
import XMonad.Conf.MyTerminalAction

------------------------------------------------------------------------------------------
-- GridSelect
------------------------------------------------------------------------------------------
hidpiGSConfig :: HasColorizer a => GSConfig a
hidpiGSConfig = (buildDefaultGSConfig nierColorizer) {
                  gs_cellheight = 80
                , gs_cellwidth = 800
                , gs_font = "xft:monospace-9:bold,Symbola-9:bold"
                , gs_navigate   = myNavNSearch
}

nierColorizer :: a-> Bool -> X (String, String)
nierColorizer a active =
  if active then
      return (black, white)
  else
      return (gray, black)

spawnAppSelected conf apps = gridselect conf apps >>= doForJust spawn

runActionSelected conf actions = gridselect conf actions >>= doForJust (\x -> x)

myNavNSearch :: TwoD a (Maybe a)
myNavNSearch = makeXEventhandler $ shadowWithKeymap navNSearchKeyMap navNSearchDefaultHandler
  where navNSearchKeyMap = M.fromList [
           ((0,xK_Escape)     , cancel)
          ,((controlMask,xK_g), cancel)
          ,((0,xK_Return)     , select)
          ,((0,xK_Left)       , move (-1,0) >> myNavNSearch)
          ,((controlMask, xK_b)       , move (-1,0) >> myNavNSearch)
          ,((0,xK_Right)      , move (1,0) >> myNavNSearch)
          ,((controlMask, xK_f)       , move (1,0) >> myNavNSearch)
          ,((0,xK_Down)       , move (0,1) >> myNavNSearch)
          ,((controlMask, xK_n)       , move (0,1) >> myNavNSearch)
          ,((0,xK_Up)         , move (0,-1) >> myNavNSearch)
          ,((controlMask, xK_p)       , move (0,-1) >> myNavNSearch)
          ,((controlMask, xK_a)       , move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> move (-1,0) >> myNavNSearch)
          ,((controlMask, xK_e)       , move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> move (1,0) >> myNavNSearch)
          ,((0,xK_Tab)        , moveNext >> myNavNSearch)
          ,((shiftMask,xK_Tab), movePrev >> myNavNSearch)
          ,((0,xK_BackSpace), transformSearchString (\s -> if (s == "") then "" else init s) >> myNavNSearch)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navNSearchDefaultHandler (_,s,mask) = do
          if mask == 0 then
            transformSearchString (++ s)
            >> myNavNSearch
          else
            myNavNSearch

scratchpadSelected :: GSConfig NamedScratchpad -> [NamedScratchpad] -> X()
scratchpadSelected config scratchpads = do
    scratchpadMaybe <- gridselect config (map (\s -> (name s, s)) scratchpads)
    myNamedScratchpadActionMaybe scratchpadMaybe

anyWorkspacePredicate :: WindowSet -> WindowSpace -> Bool
anyWorkspacePredicate windowset workspace = ("NSP" :: WorkspaceId) /= (W.tag workspace)
anyWorkspaceInCurrentWorkspaceFamilyPredicate :: WindowSet -> WindowSpace -> Bool
anyWorkspaceInCurrentWorkspaceFamilyPredicate windowset workspace = anyWorkspacePredicate windowset workspace && ((toFamilyIdMaybe $ W.currentTag windowset) == (toFamilyIdMaybe $ W.tag workspace))
visibleWorkspacesPredicate :: WindowSet -> WindowSpace -> Bool
visibleWorkspacesPredicate windowset workspace = anyWorkspacePredicate windowset workspace && ((W.tag workspace == W.currentTag windowset) || (L.elem (W.tag workspace) $ L.map (W.tag . W.workspace) $ W.visible windowset))

goToSelected' :: (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X ()
goToSelected' =
    withSelectedWindow' $ \w -> do
      s <- gets windowset
      case W.findTag w s of
        Just tag -> windows $ (W.focusWindow w) . (W.greedyView tag)
        Nothing -> windows $ W.focusWindow w

shiftSelected' :: (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X ()
shiftSelected' =
    withSelectedWindow' $ \w -> (windows $ \s -> W.shiftMaster $ W.focusWindow w $ W.shiftWin (W.currentTag s) w s)

-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWindow' :: (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X (Maybe Window)
gridselectWindow' predicate gsconf = windowMap' predicate >>= gridselect gsconf

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow' :: (Window -> X ()) -> (WindowSet -> WindowSpace -> Bool) -> GSConfig Window -> X ()
withSelectedWindow' callback predicate conf = gridselectWindow' predicate conf >>= doForJust callback

------------------------------------------------------------------------------------------
-- GridSelect
------------------------------------------------------------------------------------------

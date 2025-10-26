
module XMonad.Util.MyUtils where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties (getProp32s)

ifX :: Bool -> X() -> X()
ifX cond whenTrue = if cond then whenTrue else return ()

xnot :: X Bool -> X Bool
xnot x = not <$> x

caseMaybeJust :: Maybe a -> (a -> X ()) -> X ()
caseMaybeJust m f =
  case m of
    Just a -> f a
    Nothing -> return ()
doForJust f m = caseMaybeJust m f

andThen cmp1 cmp2 a b = do
  let c = cmp1 a b
  if c == EQ then cmp2 a b
  else c

isDialog = ask >>= \w -> liftX $ do
  desk <- getAtom "_NET_WM_WINDOW_TYPE_DIALOG"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (== desk) (map fromIntegral rs)
    _       -> return False

viewScreen :: ScreenId -> X ()
viewScreen sid = screenWorkspace sid >>= doForJust (windows . W.view)

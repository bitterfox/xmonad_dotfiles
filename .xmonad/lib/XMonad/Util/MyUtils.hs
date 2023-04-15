
module XMonad.Util.MyUtils (
  ifX,
  caseMaybeJust,
  andThen
) where

import XMonad

ifX :: Bool -> X() -> X()
ifX cond whenTrue = if cond then whenTrue else return ()

caseMaybeJust :: Maybe a -> (a -> X ()) -> X ()
caseMaybeJust m f =
  case m of
    Just a -> f a
    Nothing -> return ()

andThen cmp1 cmp2 a b = do
  let c = cmp1 a b
  if c == EQ then cmp2 a b
  else c

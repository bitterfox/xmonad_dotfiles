
module XMonad.Util.MyUtils (
  ifX
) where

import XMonad

ifX :: Bool -> X() -> X()
ifX cond whenTrue = if cond then whenTrue else return ()

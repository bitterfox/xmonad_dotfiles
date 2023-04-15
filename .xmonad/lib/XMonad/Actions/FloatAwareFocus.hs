
module XMonad.Actions.FloatAwareFocus (
  floatAvoidFocusUp, floatAvoidFocusDown,
  floatFocusUp, floatFocusDown,
  floatAvoidSwapUp,
  floatOnUp,
  locateFloat,
  isFloat
) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Ord

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.MyUtils

------------------------------------------------------------------------------------------
-- Float aware focus
------------------------------------------------------------------------------------------

floatAvoidFocusUp, floatAvoidFocusDown :: Ord a => W.StackSet i l a s sd -> W.StackSet i l a s sd
floatAvoidFocusUp stackSet = W.modify' (floatAvoidFocusUp' stackSet) stackSet
floatAvoidFocusDown stackSet = W.modify' (floatAvoidFocusDown' stackSet) stackSet

floatAvoidFocusUp', floatAvoidFocusDown' :: Ord a => W.StackSet i l a s sd -> W.Stack a -> W.Stack a
floatAvoidFocusUp' stackSet stack@(W.Stack t (l:ls) rs) =
    if M.member l $ W.floating stackSet then
        let W.Stack t' ls' rs' = floatAvoidFocusUp' stackSet (W.Stack t ls rs) in
          W.Stack t' (l:ls') rs'
    else W.Stack l ls (t:rs)
floatAvoidFocusUp' stackSet stack@(W.Stack t [] rs) = do
    let (x:xs) = reverse rs
    if M.member x $ W.floating stackSet then do
      let W.Stack t' ls' rs' = floatAvoidFocusUp' stackSet (W.Stack t [] $ reverse xs)
      W.Stack t' (x:ls') rs'
    else
      W.Stack x (xs ++ [t]) []

--floatAvoidFocusUp' stackSet (W.Stack t ls rs) = do
--  let (lf, ls') = L.partition (isFloat stackSet) ls
--  let (rf, rs') = L.partition (isFloat stackSet) rs
--  let W.Stack t' nls nrs = W.focusUp' $ W.Stack t ls' rs'
--  W.Stack t' (lf ++ nls) (rf ++ nrs)
floatAvoidFocusUp' stackSet stack@(W.Stack t [] []) = stack

floatAvoidFocusDown' stackSet = reverseStack . (floatAvoidFocusUp' stackSet) . reverseStack

floatAvoidSwapUp stackSet = W.modify' (floatAvoidSwapUp' stackSet) stackSet
floatAvoidSwapUp' stackSet stack@(W.Stack t ls rs) =
  if isFloat stackSet t then stack
  else do
    let (lf, ls') = span (isFloat stackSet) ls
    let W.Stack t' nls nrs = swapUp' $ W.Stack t ls' rs
    W.Stack t' nls ((reverse lf) ++ nrs)

swapUp' :: W.Stack a -> W.Stack a
swapUp'  (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp'  (W.Stack t []     rs) = W.Stack t (reverse rs) []

floatFocusUp, floatFocusDown :: Ord a => W.StackSet i l a s sd -> W.StackSet i l a s sd
floatFocusUp stackSet = W.modify' (floatFocusUp' stackSet) stackSet
floatFocusDown stackSet = W.modify' (floatFocusDown' stackSet) stackSet

floatFocusUp', floatFocusDown' :: Ord a => W.StackSet i l a s sd -> W.Stack a -> W.Stack a
floatFocusUp' = floatFocusNext . reverse . sortedFloats

floatFocusDown' = floatFocusNext . sortedFloats

sortedFloats stackSet = L.map fst $ sortedFloats' stackSet
sortedFloats' stackSet =
    L.sortBy comparator floats
        where floats = M.assocs $ W.floating stackSet
              comparingX = comparing $ \(wid, W.RationalRect x y w h) -> x
              comparingY = comparing $ \(wid, W.RationalRect x y w h) -> y
              comparingW = comparing $ \(wid, W.RationalRect x y w h) -> w
              comparingH = comparing $ \(wid, W.RationalRect x y w h) -> h
              comparingWid = comparing $ \(wid, W.RationalRect x y w h) -> wid
              comparator = comparingY `andThen` comparingX `andThen` comparingH `andThen` comparingW `andThen` comparingWid

floatFocusNext floats stack@(W.Stack t ls rs) = do
    let ws = W.integrate stack
    let fs = L.reverse $ L.filter (\w -> L.elem w ws) $ floats
    if fs == [] || fs == [t] then stack
    else do
      let fs' = takeWhile (/= t) fs
      let w = if fs' == [] then last fs else last fs'
      W.Stack w (L.delete w ls) $ t:(L.delete w rs)

floatOnUp = withWindowSet(\s -> do
--  before <- gets windowset
--  caseMaybeJust (W.stack $ W.workspace $ W.current before) $
--    \(W.Stack t ls rs) -> spawn $ "echo 'Current: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
  caseMaybeJust (W.stack $ W.workspace $ W.current s) $
    \(W.Stack t ls rs) -> do
      if isFloat s t then
          if (L.filter (isFloat s) ls) == [] then return ()
          else focusedFloatOnUp
      else do
        let (rf, rs') = L.partition (isFloat s) rs
        let (lf, ls') = L.partition (isFloat s) $ L.dropWhile (isFloat s) ls
        if (rf ++ lf) == [] then return ()
        else floatOnUp')
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'Current: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ())

floatOnUp' = do
--  before <- gets windowset
--  caseMaybeJust (W.stack $ W.workspace $ W.current before) $
--    \(W.Stack t ls rs) -> spawn $ "echo 'Before: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
  modifyWindowSet (\s -> W.modify' (\stack@(W.Stack t ls rs) -> do
    let (rf, rs') = L.partition (isFloat s) $ L.reverse rs
    let lf' = L.takeWhile (isFloat s) ls
    let (lf, ls') = L.partition (isFloat s) $ L.dropWhile (isFloat s) ls
    if (rf ++ lf) == [] then stack
    else W.Stack t (rf ++ lf' ++ lf ++ ls') $ reverse rs') s)
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'After: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ()

focusedFloatOnUp = do
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'Before: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ()
  windows (\s -> W.modify' (\stack@(W.Stack t ls rs) -> do
    let (lf, ls') = L.partition (isFloat s) ls
    if lf == [] then stack
--    else W.Stack t ls' $ rs ++ (reverse lf)) s)
    else W.Stack t ls' $ (reverse lf) ++ rs) s)
--  before <- gets windowset
--  case W.stack $ W.workspace $ W.current before of
--    Just (W.Stack t ls rs) -> spawn $ "echo 'After: " ++ (show t) ++ ", " ++ (show ls) ++ ", " ++ (show rs) ++ "' >> /tmp/xmonad.debug.floating"
--    Nothing -> return ()

isFloat stackSet window = M.member window $ W.floating stackSet

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls

locateFloat managehook = withWindowSet (\s -> do
  case W.peek s of
    Just t ->
      if isFloat s t then do
        endo <- runQuery managehook t
        windows $ appEndo endo
        return True
      else
        return False
    Nothing -> return False
  )

------------------------------------------------------------------------------------------
-- Float aware focus
------------------------------------------------------------------------------------------

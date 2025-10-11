module XMonad.Util.ManageHookUtils (
  avoidStrutsFloat,
  onCenter, onCenter', onCenter'', onCenter''',
  onTop, onTop', onTop'',
  onBottom, onBottom', onBottom'',
  onLeft, onLeft', onLeft'',
  onRight, onRight', onRight'',
  minWidth
) where

import Data.Ratio
import qualified Data.List as L
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad2

import XMonad.Hooks.ManageDocks
import XMonad.Util.Types

onCenter = onCenter' 0
onCenter' spaceRatio = onCenter'' spaceRatio spaceRatio
onCenter'' spaceRatioV spaceRatioH = (avoidStrutsFloat $ W.RationalRect spaceRatioV (spaceRatioH) (1-spaceRatioV*2) (1-spaceRatioH*2))
onCenter''' spaceRatioV spaceRatioH f = (avoidStrutsFloat' (W.RationalRect spaceRatioV (spaceRatioH) (1-spaceRatioV*2) (1-spaceRatioH*2)) f)

onTop = onTop' 0
onTop' spaceRatio = onTop'' spaceRatio spaceRatio
onTop'' spaceRatioV spaceRatioH = (avoidStrutsFloat $ W.RationalRect spaceRatioV (spaceRatioH) (1-spaceRatioV*2) (0.5-spaceRatioH*2))

onBottom = onBottom' 0
onBottom' spaceRatio = onBottom'' spaceRatio spaceRatio
onBottom'' spaceRatioV spaceRatioH = (avoidStrutsFloat $ W.RationalRect spaceRatioV (0.5+spaceRatioH) (1-spaceRatioV*2) (0.5-spaceRatioH*2))

onLeft = onLeft' 0
onLeft' spaceRatio = onLeft'' spaceRatio spaceRatio
onLeft'' spaceRatioV spaceRatioH = (avoidStrutsFloat $ W.RationalRect spaceRatioV (spaceRatioH) (0.5-spaceRatioV*2) (1-spaceRatioH*2))

onRight = onRight' 0
onRight' spaceRatio = onRight'' spaceRatio spaceRatio
onRight'' spaceRatioV spaceRatioH = (avoidStrutsFloat $ W.RationalRect (0.5+spaceRatioV) (spaceRatioH) (0.5-spaceRatioV*2) (1-spaceRatioH*2))

avoidStrutsFloat r = avoidStrutsFloat' r $ \r' -> r'

avoidStrutsFloat' (W.RationalRect x y w h) f = do
  win <- ask
  (sid, r) <- liftX $ floatLocation win
  rect <- liftX $ withWindowSet $ \ws -> do
    let sc = W.current ws
    let r = screenRect $ W.screenDetail sc
    let Rectangle rx ry rw rh = r
    srect <- fmap ($ r) (calcGap $ S.fromList [U,D,L,R])
    let Rectangle sx sy sw sh = srect
    let nx = (fromIntegral sx) + (x * (fromIntegral sw))
    let ny = (fromIntegral sy) + (y * (fromIntegral sh))
    let nw = (w * (fromIntegral sw))
    let nh = (h * (fromIntegral sh))
    let W.RationalRect nx' ny' nw' nh' = f $ W.RationalRect nx ny nw nh
    return $ (W.RationalRect
                   ((nx' - (fromIntegral rx)) / (fromIntegral rw))
                   ((ny' - (fromIntegral ry)) / (fromIntegral rh))
                   (nw' / (fromIntegral rw))
                   (nh' / (fromIntegral rh))
             )
  customFloating rect

minWidth mw r@(W.RationalRect x y w h) =
   if w < mw then
      (W.RationalRect (x-(mw-w)/2) y mw h)
  else
      r


module XMonad.Util.ManageHookUtils (
  avoidStrutsFloat,
  onCenter, onCenter', onCenter'',
  onTop, onTop', onTop'',
  onBottom, onBottom', onBottom'',
  onLeft, onLeft', onLeft'',
  onRight, onRight', onRight''
) where

import Data.Ratio
import qualified Data.List as L
import qualified Data.Set  as S
import qualified Data.Map.Strict as M
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

import XMonad.Hooks.ManageDocks
import XMonad.Util.Types

onCenter = onCenter' 0
onCenter' spaceRatio = onCenter'' spaceRatio spaceRatio
onCenter'' spaceRatioV spaceRatioH = (avoidStrutsFloat $ W.RationalRect spaceRatioV (spaceRatioH) (1-spaceRatioV*2) (1-spaceRatioH*2))

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

avoidStrutsFloat (W.RationalRect x y w h) = do
  win <- ask
  (sid, r) <- liftX $ floatLocation win
  rect <- liftX $ withWindowSet $ \ws -> do
    case L.find ((sid ==) . W.screen) $ W.screens ws of
        Just sc -> do
          let r = screenRect $ W.screenDetail sc
          let Rectangle rx ry rw rh = r
          srect <- fmap ($ r) (calcGap $ S.fromList [U,D,L,R])
          let Rectangle sx sy sw sh = srect
          let nx = (fromIntegral sx) + (x * (fromIntegral sw))
          let ny = (fromIntegral sy) + (y * (fromIntegral sh))
          let nw = (w * (fromIntegral sw))
          let nh = (h * (fromIntegral sh))
          return $ (W.RationalRect
                                ((nx - (fromIntegral rx)) / (fromIntegral rw))
                                ((ny - (fromIntegral ry)) / (fromIntegral rh))
                                (nw / (fromIntegral rw))
                                (nh / (fromIntegral rh))
                   )
        Nothing -> return $ (W.RationalRect x y w h)
  customFloating rect

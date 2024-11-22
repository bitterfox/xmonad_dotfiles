{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module XMonad.Layout.AndroidLikeWindowView (
  AndroidLikeWindowView(..)
) where

import XMonad
import qualified XMonad.StackSet as W

data AndroidLikeWindowView a = AndroidLikeWindowView {
      spaceRatio :: !Rational
    , spaceRatioInc :: !Rational
    , gapRatioWindows :: !Rational
    , deltaHeightRatio :: !Rational
    } deriving ( Read, Show )

instance LayoutClass AndroidLikeWindowView a where
    pureLayout l rect (W.Stack focus up down) = (layoutFocus l rect focus) ++ (layoutUp l rect up) ++ (layoutDown l rect down)

    pureMessage (AndroidLikeWindowView r i g d) m = case fromMessage m of
                                                    Just Shrink -> Just $ AndroidLikeWindowView (r + i) i g d
                                                    Just Expand -> Just $ AndroidLikeWindowView (r - i) i g d
                                                    _ -> Nothing

    description (AndroidLikeWindowView r i g d) = "AndroidLikeWindowView:" ++ (show r) ++ "," ++ (show g) ++ "," ++ (show d)

spaceWidth (AndroidLikeWindowView spaceRatio _ _ _) (Rectangle _ _ w _) = floor $ fromIntegral w * spaceRatio
spaceHeight (AndroidLikeWindowView spaceRatio _ _ _) (Rectangle _ _ _ h) = floor $ fromIntegral h * spaceRatio
gapWidth (AndroidLikeWindowView _ _ gapRatio _) (Rectangle _ _ w _) = floor $ fromIntegral w * gapRatio
deltaHeight (AndroidLikeWindowView _ _ _ deltaHeightRatio) (Rectangle _ _ _ h) = floor $ fromIntegral h * deltaHeightRatio

layoutFocus l@(AndroidLikeWindowView frac _ spaceFrac spaceHeightFrac) r@(Rectangle x y w h) focus =
    [(focus, Rectangle (x + fromIntegral sw) (y + fromIntegral sh) (w - sw * 2) (h - sh * 2))]
    where sw = spaceWidth l r
          sh = spaceHeight l r

layoutUp l r@(Rectangle x y w h) (left:xs) =
    [(left, Rectangle
              (x + fromIntegral sw - fromIntegral width - fromIntegral gw)
              (y + fromIntegral sh + fromIntegral dh)
              (width)
              (height))]
    where sw = spaceWidth l r
          sh = spaceHeight l r
          gw = gapWidth l r
          dh = deltaHeight l r
          width = w - sw * 2
          height = h - sh * 2 - (dh * 2)
layoutUp l r [] = []

layoutDown l r@(Rectangle x y w h) (right:xs) =
    [(right, Rectangle
               (x + fromIntegral sw + fromIntegral width + fromIntegral gw)
               (y + fromIntegral sh + fromIntegral dh)
               (width)
               (height))]
    where sw = spaceWidth l r
          sh = spaceHeight l r
          gw = gapWidth l r
          dh = deltaHeight l r
          width = w - sw * 2
          height = h - sh * 2 - (dh * 2)
layoutDown l r [] = []


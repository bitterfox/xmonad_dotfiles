{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module XMonad.Layout.CachedLayout (
  cachedLayout,
  CachedLayout(..),
  LayoutCache(..)
) where

import qualified Data.List as L

import XMonad
import qualified XMonad.StackSet as W

data LayoutCache a = LayoutCache {
  rect :: Rectangle,
  cachedRects :: [(a, Rectangle)]
} deriving ( Read, Show )

cachedLayout l = CachedLayout l Nothing

data CachedLayout layout a = CachedLayout (layout a) (Maybe (LayoutCache a)) deriving ( Read, Show )

instance (Show a, Eq a, LayoutClass l a) => LayoutClass (CachedLayout l) a where
    runLayout (W.Workspace i (CachedLayout l mcache) ms) r = do
      let mrs = case (mcache, ms) of
                 (Just cache, Just stack) -> if (r == rect cache) && ((L.map fst $ cachedRects cache) == W.integrate stack) then Just $ cachedRects cache
                                             else Nothing
                 _ -> Nothing
      case mrs of
        Just rs -> return (rs, Nothing)
        Nothing -> do
          (ws, ml) <- runLayout (W.Workspace i l ms) r
          case ml of
            Just nl -> return (ws, Just $ cachedLayout nl)
            Nothing -> return (ws, Just $ CachedLayout l $ Just $ LayoutCache r ws)

    handleMessage (CachedLayout l _) mess = do
      ml <- handleMessage l mess
      return $ cachedLayout <$> ml

    description (CachedLayout l _) = description l


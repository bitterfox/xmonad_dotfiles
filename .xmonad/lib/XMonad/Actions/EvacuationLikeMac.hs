
module XMonad.Actions.EvacuationLikeMac (
  showOrHideScratchpads,
  evacuateWindowsLikeMac
) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Concurrent
import Control.Monad (filterM)

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedScratchpad

------------------------------------------------------------------------------------------
-- Evacuation
------------------------------------------------------------------------------------------
data MaybeWindowLocationMap = MaybeWindowLocationMap (M.Map Window (W.RationalRect, (Rational, Rational))) deriving Typeable
instance ExtensionClass MaybeWindowLocationMap where
  initialValue = MaybeWindowLocationMap M.empty

showOrHideScratchpads scratchpads show =
    withWindowSet(\s -> do
--      filtered <- windowsMatchScratchpads s
      toMoveActions s (currentWindows s)
--      toMoveActions s filtered
    )
  where
    acceleration = 0.001
    delay = 5
    toShowActions = \s ws -> do
                      MaybeWindowLocationMap evacuateds <- XS.get
                      let
                        currents = W.floating s;
                        toBeShowns = mapMaybe (\w -> do
                          current <- M.lookup w currents
                          (W.RationalRect x y width height, (_x, _y))  <- M.lookup w evacuateds
                          Just (w, current, (x, y))
                          ) ws
--                      io $ appendFile "/tmp/xmonad.debug" "show\n"
--                      io $ appendFile "/tmp/xmonad.debug" $ TS.show toBeShowns
                      dynamicMoving toBeShowns acceleration delay
                      XS.put $ MaybeWindowLocationMap $ L.foldl (\m (w, c, t) -> M.delete w m) evacuateds toBeShowns
    toHideActions = \s ws -> do
                      MaybeWindowLocationMap evacuateds <- XS.get
                      let
                        currents = W.floating s
                        toBeEvacuateds = mapMaybe (\w -> do
                          r <- M.lookup w currents
                          let W.RationalRect x y width height = r
                          case M.lookup w evacuateds of
                            Nothing -> Just (w, r)
                            Just (original, e)
                              | ((x, y) == e) -> Nothing -- (r == original) || 
                              | otherwise -> Just (w, r)
                          ) ws
--                      io $ appendFile "/tmp/xmonad.debug" "hide\n"
--                      io $ appendFile "/tmp/xmonad.debug" $ TS.show toBeEvacuateds
                      newLocationMap <- evacuateWindowsLikeMac toBeEvacuateds acceleration delay
                      let
                        evacuatedWindowLocations = mapMaybe (\(w, c) -> do
                          newLocation <- M.lookup w newLocationMap
                          Just (w, c, newLocation)
                          ) toBeEvacuateds
                      evacuatedLocations <- return (mapMaybe (\(w, c) -> do
                        W.RationalRect x y width height <- M.lookup w $ W.floating s
                        Just (w, c, (x, y))
                        ) toBeEvacuateds)
                      XS.put $ MaybeWindowLocationMap $ L.foldl (\m (w, c, t) -> M.insert w (c, t) m) evacuateds evacuatedWindowLocations
    toMoveActions = if show then toShowActions else toHideActions
    currentWindows = maybe [] W.integrate . W.stack . W.workspace . W.current -- ws -> [Window]
    queries = map query scratchpads
    isMatchScratchpads = \w ->
        L.foldl (<||>) (return False) (L.map (\ns -> runQuery (query ns) w) scratchpads)
    windowsMatchScratchpads = \s -> filterM isMatchScratchpads $ currentWindows s -- WindowSet -> X [Window]
--    hideHook = \w -> return (customFloating $ W.RationalRect (-1000) (-1000) 0 0)
--    toHideActions = \s ws -> dynamicMoving (mapMaybe (\(f, w) ->
--                      do
--                        fromRect <- M.lookup w (W.floating s)
--                        let W.RationalRect x y width height = fromRect
--                        Just (f w fromRect)
--                      ) (L.zip hideActionFactories ws)) 0.001 10
--    hideActionFactories = cycle [toLeft, toCenter, toRight]
--    toCenter = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (x, 0.03-height))
--    toLeft = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (0.03-width, 0.03-height))
--    toRight = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (0.97, (0.03-height)))
--    toCenter = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (x, 0.99))
--    toLeft = \w rect -> let W.RationalRect x y width height = rect in (w, rect, ((0.01-width), 0.99))
--    toRight = \w rect -> let W.RationalRect x y width height = rect in (w, rect, (0.99, 0.99))

-- toggleFloatingWindowsEvacuation :: X ()
-- toggleFloatingWindowsEvacuation =

evacuateWindowsLikeMac :: [(Window, W.RationalRect)] -> Rational -> Int-> X (M.Map Window (Rational, Rational))
evacuateWindowsLikeMac l acceleration delay = do
    let withTo = L.map computeTo l
    dynamicMoving withTo acceleration delay
    return $ L.foldl (\m (w, c, t) -> M.insert w t m) M.empty withTo
  where
    computeTo = \(w, r) -> (w, r, to r)
    delta = 0.02
    topX = \(W.RationalRect x y w h) -> delta-w
    topY = \(W.RationalRect x y w h) -> delta+0.02-h
    bottomX = \(W.RationalRect x y w h) -> 1-delta
    bottomY = \(W.RationalRect x y w h) -> 1-delta
    dx = \(W.RationalRect x y w h) -> x + (w/2)
    dy = \(W.RationalRect x y w h) -> y + (h/2)
    fx = \r x -> if (dx r) == 0.5 then let W.RationalRect _x _y w h = r in _y else
          let
            a = ((dy r)-0.5)/((dx r)-0.5)
            W.RationalRect _x _y w h = r
          in
            a*(x+(w/2))+0.5*(1-a)-(h/2)
    fy = \r y -> if (dy r) == 0.5 then let W.RationalRect _x _y w h = r in _x else
          let
            a = ((dx r)-0.5)/((dy r)-0.5)
            W.RationalRect _x _y w h = r
          in
            a*(y+(h/2))+0.5*(1-a)-(w/2)
    leftTop     = 0
    rightTop    = 1
    leftBottom  = 2
    rightBottom = 3
    whereIs = \r ->
                if (dy r) <= 0.5 then -- Top
                  if (dx r) <= 0.5 then leftTop else rightTop
                else
                  if (dx r) <= 0.5 then leftBottom else rightBottom
    to = \r -> let W.RationalRect x y w h = r in
--                 l = whereIs r
--               in
--                 if l == leftTop then
--                   let _y = fx r $ topX r in
--                     if _y+h >= 0 then (topX r, _y) else (fy r $ topY r, topY r)
--                 else if l == rightTop then
--                   let _y = fx r $ bottomX r in
--                     if _y+h >= 0 then (bottomX r, _y) else (fy r $ topY r, topY r)
--                 else if l == leftBottom then
--                   let _y = fx r $ topX r in
--                     if (_y+h >= 0) && (_y < bottomY r) then (topX r, _y) else (fy r $ bottomY r, bottomY r)
--                 else
--                   let _y = fx r $ bottomX r in
--                     if (_y+h >= 0) && (_y < bottomY r) then (bottomX r, _y) else (fy r $ bottomY r, bottomY r)
          if ((dx r) == 0.5) || (abs (((dy r) - 0.5) / ((dx r) - 0.5)) > 1) then
            if (dx r) == 0.5 then (x, if (dy r) <= 0.5 then topY r else bottomY r) else
            if (dy r) <= 0.5 then
              (fy r $ topY r, topY r)
            else
              let _x = fy r $ bottomY r in
                if (_x+w > 0) && (_x < 1) then
                    (_x, bottomY r)
                else
                  if (dx r) <= 0.5 then (topX r, fx r $ topX r)
                  else (bottomX r, fx r $ bottomX r)
          else
            if (dy r) == 0.5 then (if (dx r) <= 0.5 then topX r else bottomX r, y) else
            if (dx r) <= 0.5 then
              let _y = fx r $ topX r in
                if (_y+h > 0) && (_y < 1) then
                  (topX r, _y)
              else
                if (dy r) <= 0.5 then (fy r $ topY r, topY r)
                else (fy r $ bottomY r, bottomY r)
            else
              (bottomX r, fx r $ bottomX r)

dynamicMoving :: [(Window, W.RationalRect, (Rational, Rational))] -> Rational -> Int -> X ()
dynamicMoving l acceleration delay =
    moving (directionize l) 0
  where
    directionize = map directionizeMapper
    directionizeMapper = \(w, r, (toX, toY)) ->
                           let
                             W.RationalRect x y width height = r
                             dirX = if x <= toX then right else left
                             dirY = if y <= toY then down else up
                           in
                             (w, r, (toX, toY), (dirX, dirY))
    up = -1
    down = 1
    left = -1
    right = 1
    checkDoneAll = \l -> L.foldr (&&) True $ L.map checkDone l
    checkDone = \t -> (&&) (doneX t) (doneY t)
    doneX = \(w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) -> ((toX-x)*dirX)<=0
    doneY = \(w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) -> ((toY-y)*dirY)<=0
    moving = \l speed ->
               if checkDoneAll l then
                 moveAll (L.map forceToLocation l)
               else
                 let
                   newList = L.map (\t ->
                     let
                       (w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) = t
                       k = 1
                       diffX = abs(x-toX)
                       diffY = abs(y-toY)
                       newX = if doneX t then toX else x+(speed*k*dirX)*(diffX/(diffX+diffY))
                       newY = if doneY t then toY else y+(speed*k*dirY)*(diffY/(diffX+diffY))
                     in
                       (w, W.RationalRect newX newY width height, (toX, toY), (dirX, dirY))) l
                 in
                   (whenX (return $ delay /= 0) $ io $ threadDelay delay)
                   >> moveAll (L.map (
                     \t -> if checkDone t then forceToLocation t else
                       let (w, to, (toX, toY), (dirX, dirY)) = t in (w, to)
                     ) newList
                   )
                   >> moving newList (speed+acceleration)
    forceToLocation = \(w, W.RationalRect x y width height, (toX, toY), (dirX, dirY)) -> (w, W.RationalRect toX toY width height)
    moveAll = \l -> windows (\s ->
                L.foldl (\ss (w, r) -> W.float w r ss) s l
              )
    move = \(w, newRect) -> windows $ W.float w newRect
------------------------------------------------------------------------------------------
-- Evacuation
------------------------------------------------------------------------------------------

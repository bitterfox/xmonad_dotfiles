{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module XMonad.Layout.CompositeTall (
  CompositeTall(..), CompositeCell(..),
  compositeTall,
  NewCell(..),
  DelegateMessage(..),
  ResizeAnotherSide(..),
  ResetSize(..),
  SimpleWide(..),
  simpleWide
) where

import qualified Data.List as L
import Data.Maybe
import Control.Monad (msum)

import XMonad
import qualified XMonad.StackSet as W

data CompositeTall layout a = CompositeTall {
      compositeTallCells :: [CompositeCell layout a],
      compositeTallRatioIncrement :: Rational,
      compositeTallLayoutTemplate :: layout a,
      compositeTallRestLayout :: layout a
  } deriving ( Read, Show )

data CompositeCell layout a = CompositeCell {
      compositeCellWindows :: Int,
      compositeCellRatio :: Rational,
      compositeCellLayout :: layout a
  } deriving ( Read, Show )

compositeTall ratioIncrement layoutTemplate = CompositeTall [CompositeCell 1 1 layoutTemplate] ratioIncrement layoutTemplate layoutTemplate

data NewCell = NewCellAtLeft | NewCellAtRight deriving ( Typeable )
instance Message NewCell

data DelegateMessage = DelegateMessage SomeMessage deriving ( Typeable )
instance Message DelegateMessage

data ResizeAnotherSide = ResizeAnotherSide Resize deriving ( Typeable )
instance Message ResizeAnotherSide

data ResetSize = ResetSize deriving ( Typeable )
instance Message ResetSize

instance (LayoutClass l a, Show a, Eq a) => LayoutClass (CompositeTall l) a where
    runLayout (W.Workspace tag layout stackMaybe) rect =
      case stackMaybe of
        Just stack -> do
--      let list = assignWindows layout windows
          let list = assignStack layout stack
          let rects = splitRect list rect
--          spawn $ "echo '" ++ (show rect) ++ "' >> /tmp/xmonad.debug.layout"
--          spawn $ "echo '" ++ (show (L.map (\(wins, r, _) -> (wins, r)) list)) ++ "' >> /tmp/xmonad.debug.layout"
--          spawn $ "echo '" ++ (show (L.map (\(wins, r, _) -> (wins, r)) rects)) ++ "' >> /tmp/xmonad.debug.layout"
          results <- applyLayouts rects tag stack
--          spawn $ "echo '" ++ (show results) ++ "' >> /tmp/xmonad.debug.layout"
          return (fst results,
            if L.all isNothing $ snd results then Nothing
            else Just $ replaceLayouts layout $ snd results)
--          return (L.map (\(wins, r, layout) -> (head wins, r)) rects, Nothing)
--      return ([(W.focus stack, rect)], Nothing)
        Nothing -> return ([], Nothing)

    handleMessage layout m = do
      maybeCells <- stackCells layout
      maybeLayouts <- stackLayouts layout
      let maybeNewLayout = msum [fmap (handleResize maybeCells) (fromMessage m)
                           ,fmap (handleResizeAnotherSide maybeCells maybeLayouts) (fromMessage m)
                           ,fmap handleResetSize (fromMessage m)
                           ,fmap (handleIncMasterN maybeCells) (fromMessage m)
                           ,fmap (handleNewCellMessage maybeLayouts) (fromMessage m)]
      case maybeNewLayout of
        Just newLayout ->
          case fromMessage m of
            Just ResetSize ->
              handleDelegateMessage newLayout maybeLayouts m >>= (return . Just . head . catMaybes . (:[maybeNewLayout]))
            _ -> return maybeNewLayout
        Nothing -> case fromMessage m of
            Just (DelegateMessage m) -> handleDelegateMessage layout maybeLayouts m
            _ -> handleDelegateMessage layout maybeLayouts m
      where
        handleResize (Just cells) Shrink = layout {compositeTallCells = replaceFocusedCell cells $ \c -> c {compositeCellRatio = max 0 $ compositeCellRatio c - compositeTallRatioIncrement layout}}
        handleResize (Just cells) Expand = layout {compositeTallCells = replaceFocusedCell cells $ \c -> c {compositeCellRatio = compositeCellRatio c + compositeTallRatioIncrement layout}}
        handleResizeAnotherSide (Just cells) (Just layouts) (ResizeAnotherSide resize) =
          if (L.null $ W.down layouts) || (L.null $ W.up cells) then
            handleResize (Just cells) $ reverseResize resize
          else do
            let up = head $ W.up cells
            let f = W.focus cells
            let ratioInc = compositeTallRatioIncrement layout
            let diff = case resize of
                         Expand -> min (compositeCellRatio up) ratioInc
                         Shrink -> min (compositeCellRatio f) ratioInc
            let replacedCells = cells {
                                  W.focus = f {
                                     compositeCellRatio = (case resize of
                                                             Expand -> (+)
                                                             Shrink -> (-)) (compositeCellRatio f) diff
                                  },
                                  W.up = (up {
                                     compositeCellRatio = (case resize of
                                                             Expand -> (-)
                                                             Shrink -> (+)) (compositeCellRatio up) diff
                                  }):(tail $ W.up cells)
                                }
            layout {
                 compositeTallCells = W.integrate replacedCells
            }
        handleResetSize ResetSize =
          layout {
            compositeTallCells = L.map (\c -> c {compositeCellRatio = 1}) $ compositeTallCells layout
          }
        handleIncMasterN (Just cells) (IncMasterN n) = do
          let newWins = (compositeCellWindows $ W.focus cells) + n
          if newWins <= 0 then
            layout {compositeTallCells = removeFocusedCell cells}
          else
            layout {compositeTallCells = replaceFocusedCell cells $ \c -> c { compositeCellWindows = newWins }}
        handleIncMasterN Nothing (IncMasterN n) = do
          if n > 0 then
            layout {compositeTallCells = [CompositeCell n 1 $ compositeTallLayoutTemplate layout]}
          else
            layout
        handleNewCellMessage (Just layouts) NewCellAtLeft = do
          let len = L.length $ W.up layouts
          let (ls,rs) = L.splitAt len $ compositeTallCells layout
          layout {compositeTallCells = ls ++ [CompositeCell 1 1 $ compositeTallLayoutTemplate layout] ++ rs}
        handleNewCellMessage (Just layouts) NewCellAtRight = do
          let len = L.length $ W.up layouts
          let (ls,rs) = L.splitAt (len+1) $ compositeTallCells layout
          layout {compositeTallCells = ls ++ [CompositeCell 1 1 $ compositeTallLayoutTemplate layout] ++ rs}
        handleDelegateMessage currentLayout (Just layouts) m = do
              ml <- handleMessage (W.focus layouts) m
              return (ml >>= (\l -> Just $ layouts {
                                      W.focus = Just l,
                                      W.up = L.map (\n -> Nothing) (W.up layouts),
                                      W.down = L.map (\n -> Nothing) (W.down layouts)
                                    })
                         >>= (Just . (replaceLayouts currentLayout) . W.integrate))
    description = show

data SimpleWide a = SimpleWide {
      simpleWideRatio :: [Rational],
      simpleWideRatioIncrement :: Rational,
      simpleWideRatioCurrentIndex :: Int,
      simpleWideRatioCurrentLength :: Int,
      simpleWideRectCache :: Maybe (Rectangle, [Rectangle])
} deriving ( Read, Show )

simpleWide ratioIncrement = SimpleWide [] ratioIncrement 0 0 Nothing

instance LayoutClass SimpleWide a where
    doLayout l@(SimpleWide ratio _ cidx clen mcache) rect stack = do
      let rs = ratio ++ (L.replicate (len - L.length ratio) 1)
      let mrs = case mcache of
                 Just (r, cache) -> if clen == len && rect == r then Just cache else Nothing
                 Nothing -> Nothing
      let rects = case mrs of
                 Just rs -> L.zip wins rs
                 Nothing -> do
                   let list = zipWith (\w r -> ([w], r, l)) wins rs
                   let rects = splitRect list $ mirrorRect rect
                   L.map (\(ws, r, l) -> (head ws, mirrorRect r)) rects
      return (rects,
        modifyLayout [
          (\layout -> if cidx == idx then Nothing
                      else Just $ layout {simpleWideRatioCurrentIndex = idx})
        , (\layout -> if rs == ratio then Nothing
                      else Just $ layout {simpleWideRatio = rs})
        , (\layout -> if clen == len then Nothing
                      else Just $ layout {simpleWideRatioCurrentLength = len})
        , (\layout -> if isNothing mrs then Just $ layout {simpleWideRectCache = Just (rect, L.map snd rects)}
                      else Nothing)
        ] l)
      where idx = L.length $ W.up stack
            wins = W.integrate stack
            len = L.length wins
    pureMessage (SimpleWide ratio ratioInc cidx clen _) m = do
        msum [fmap (handleResize l cidx) (fromMessage m)
             ,fmap handleResizeAnotherSide (fromMessage m)
             ,fmap handleResetSize (fromMessage m)]
      where l = SimpleWide ratio ratioInc cidx clen Nothing -- Evict cache
            rs layout = (simpleWideRatio layout) ++ (L.replicate (cidx - (L.length $ simpleWideRatio layout)) 1)
            handleResize layout i Shrink = do
              let (f, s) = splitAt (min (clen -2) i) $ rs layout
              if L.null s then layout
              else layout { simpleWideRatio = f ++ ((max 0 $ head s - ratioInc):(tail s)) }
            handleResize layout i Expand = do
              let (f, s) = splitAt (min (clen -2) i) $ rs layout
              if L.null s then layout
              else layout { simpleWideRatio = f ++ ((head s + ratioInc):(tail s)) }
            handleResizeAnotherSide (ResizeAnotherSide resize) =
                if (cidx == 0) || (cidx == clen - 1) then
                    handleResize l cidx $ reverseResize resize
                else do
                  let (f, s) = splitAt (min (clen -2) cidx-1) $ rs l
                  let up = head s
                  let down = head $ tail s
                  let diff = case resize of
                               Expand -> min up ratioInc
                               Shrink -> min down ratioInc
                  case resize of
                    Expand -> l { simpleWideRatio = f ++ [up - diff, down + diff] ++ (tail $ tail s) }
                    Shrink -> l { simpleWideRatio = f ++ [up + diff, down - diff] ++ (tail $ tail s) }
            handleResetSize ResetSize = l {simpleWideRatio = L.map (\r -> 1) ratio}

    description = show

reverseResize Shrink = Expand
reverseResize Expand = Shrink

modifyLayout fs defaultLayout = modifyLayout' fs Nothing defaultLayout
modifyLayout' (f:fs) maybeLayout defaultLayout = do
  let modifiedLayoutMaybe = f (case maybeLayout of
                                 Just l -> l
                                 Nothing -> defaultLayout)
  modifyLayout' fs (if isNothing modifiedLayoutMaybe then maybeLayout else modifiedLayoutMaybe) defaultLayout
modifyLayout' [] maybeLayout defaultLayout = maybeLayout

rebuildStack wins stack = do
  let (f, s) = span (W.focus stack /=) wins
  if L.null s then W.Stack (head wins) [] (L.tail wins)
  else W.Stack (head s) (L.reverse f) $ L.tail s
applyLayouts :: (LayoutClass layout a, Eq a) => [([a], Rectangle, layout a)] -> WorkspaceId -> W.Stack a -> X ([(a, Rectangle)], [Maybe (layout a)])
applyLayouts [] tag stack = return $ ([], [])
applyLayouts ((wins, rect, layout):list) tag stack = do
  (f, s) <- runLayout (W.Workspace tag layout $ Just $ rebuildStack wins stack) rect
  (fx, sx) <- applyLayouts list tag stack
  return ((f++fx), (s:sx))

stackCells :: CompositeTall layout a -> X (Maybe (W.Stack (CompositeCell layout a)))
stackCells layout = do
  let cellsLen = L.length $ compositeTallCells layout
  if cellsLen == 0 then return Nothing
  else
    withWindowSet $ \ws ->
      case W.stack $ W.workspace $ W.current ws of
        Just stack -> do
          let list = assignStack layout (stack {W.down = []})
          let (f, s) = splitAt ((min cellsLen $ L.length list) - 1) $ compositeTallCells layout
          if L.null s then return Nothing
          else do
            let focus = head s
            return $ Just $ W.Stack focus (reverse f) $ tail s
        Nothing -> do
          let cells = compositeTallCells layout
          return $ Just $ W.Stack (head cells) [] (tail cells)
stackLayouts :: CompositeTall layout a -> X (Maybe (W.Stack (layout a)))
stackLayouts layout = do
  let cells = compositeTallCells layout
  let layouts = (L.map compositeCellLayout cells) ++ [compositeTallRestLayout layout]
  withWindowSet $ \ws ->
    case W.stack $ W.workspace $ W.current ws of
      Just stack -> do
        let list = assignStack layout (stack {W.down = []})
        let (f, s) = splitAt ((min (L.length layouts) $ L.length list) - 1) $ layouts
        if L.null s then return Nothing
        else do
          let focus = head s
          return $ Just $ W.Stack focus (reverse f) $ tail s
      Nothing -> do
        return $ Just $ W.Stack (head layouts) [] (tail layouts)
replaceFocusedCell :: W.Stack (CompositeCell layout a) -> (CompositeCell layout a -> CompositeCell layout a) -> [CompositeCell layout a]
replaceFocusedCell stack@W.Stack{W.focus = fc} f = W.integrate $ stack {W.focus = f fc}
removeFocusedCell stack = (L.reverse $ W.up stack) ++ W.down stack

replaceLayouts layout@CompositeTall {
                     compositeTallCells = cells,
                     compositeTallRestLayout = layoutRest
               } layouts =
  layout {
    compositeTallCells = newCells,
    compositeTallRestLayout = case newLayoutRest of
                                Just l -> l
                                Nothing -> layoutRest
  }
  where newCells = L.zipWith (\c ml -> case ml of
                                  Just l -> c {compositeCellLayout = l}
                                  Nothing -> c) cells (layouts ++ repeat Nothing)
        newLayoutRest = head $ (L.drop (L.length cells) layouts) ++ [Nothing]

assignStack layout stack = assignWindows layout $ W.integrate stack
assignWindows :: CompositeTall layout a -> [b] -> [([b], Rational, layout a)]
assignWindows layout [] = []
assignWindows layout@(CompositeTall cells inc template rest) windows =
  if L.null cells then
    [(windows, -1, rest)]
  else do
    let CompositeCell wins ratio layout = head cells
    let (f, s) = splitAt wins windows
    (f, ratio, layout):(assignWindows (CompositeTall (tail cells) inc template rest) s)

splitRect :: [([a], Rational, layout a)] -> Rectangle -> [([a], Rectangle, layout a)]
splitRect list rect = splitRect' list rect (L.length list) 0
splitRect' :: [([a], Rational, layout a)] -> Rectangle -> Int -> Dimension -> [([a], Rectangle, layout a)]
splitRect' ((wins, ratio, layout):list) rect len currentWidth =
    if L.null list then
        [(wins, rect {rect_x = x, rect_width = mw - currentWidth}, layout)]
    else do
      let w = (truncate ((fromIntegral width) * ratio))
      if currentWidth + fromIntegral w >= fromIntegral mw then
          [(wins, (rect {rect_x = x, rect_width = mw - currentWidth}), layout)] ++ (splitRect' list rect len $ mw)
      else
          (wins, (rect {rect_x = x, rect_width = fromIntegral w}), layout):(splitRect' list rect len $ currentWidth + w)
    where x = rect_x rect + (fromIntegral currentWidth)
          mw = rect_width rect
          width = (fromIntegral $ mw) `div` len
splitRect' [] rect len cw = []


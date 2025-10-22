
module XMonad.Actions.DrawShape (
  drawShapeOnMouse,
  drawShapeEventHook,
  removeAllDrawnShapes,
  removeLatestDrawnShape,
  red_rgb, blue_rgb,
  Shape(..),
  RGB(..),
  DrawShape(..),

  -- For advanced users
  DrawnShape(..),
  drawShape,
  removeAllDrawnShapes',
  redrawAllDrawnShapes',
) where

import Data.Bits (shiftL)
import Data.Maybe
import Data.Monoid
import qualified Data.List as L

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Types
import Graphics.X11.Xlib.Extras

import XMonad
import XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.MyUtils
import XMonad.Prompt (mkUnmanagedWindow)

data DrawnWindow = DrawnWindow {
  win :: Window,
  winRect :: Rectangle
} deriving (Typeable, Show)

data DrawnShape = DrawnShape {
  drawnWindows :: [DrawnWindow],
  drawnShape :: DrawShape
} deriving (Typeable, Show)

data DrawnShapes = DrawnShapes {
  drawnShapes :: [DrawnShape],
  start :: Maybe (Position, Position),
  cur :: Maybe Cursor
} deriving (Typeable, Show)

instance ExtensionClass DrawnShapes where
  initialValue = DrawnShapes {
                   drawnShapes = [],
                   start = Nothing,
                   cur = Nothing
                 }

data DrawShape = DrawShape {
  shape :: Shape,
  width :: Int,
  color :: RGB
} deriving (Typeable, Show)

data Shape = OutlinedRectangle | FilledRectangle | LongestStraightLine
             deriving (Typeable, Show)

data RGB = RGB {
      rgb_red :: Int,
      rgb_green :: Int,
      rgb_blue :: Int
} deriving (Typeable, Show)

red_rgb = RGB { rgb_red = 255, rgb_green = 0, rgb_blue = 0 }
blue_rgb = RGB { rgb_red = 0, rgb_green = 0, rgb_blue = 255 }

drawShapeOnMouse :: DrawShape -> X ()
drawShapeOnMouse ds = do
  dss :: DrawnShapes <- XS.get
  xconf <- ask
  (ok, _, _, x, y, _, _, _) <- io $ queryPointer (display xconf) (theRoot xconf)
  ifX ok $ do
      case start dss of
        Just (x', y') -> do
            let rect = Rectangle {
                           rect_x = (min (fromIntegral x) x'),
                           rect_y = (min (fromIntegral y) y'),
                           rect_width = (fromIntegral $ abs $ (fromIntegral x)-x'),
                           rect_height = (fromIntegral $ abs $ (fromIntegral y)-y')
                         }
            drawnShape <- drawShape ds rect
            XS.put $ dss {
                      drawnShapes = drawnShape:(drawnShapes dss),
                      start = Nothing
                    }
            case cur dss of
              Just c ->
                  withDisplay $ \dpy -> liftIO $ ungrabCursorForDraw dpy c
              _ -> return ()
        Nothing -> do
             cursor <- withDisplay $ \dpy -> do
                         rootw <- asks theRoot
                         liftIO $ grabCursorForDraw dpy rootw
             XS.put $ dss {
                      start = Just (fromIntegral x, fromIntegral y),
                      cur = Just cursor
                    }

drawShape :: DrawShape -> Rectangle -> X DrawnShape
drawShape ds r =
    case shape ds of
      OutlinedRectangle -> drawOutlinedRectangle ds r
      FilledRectangle -> drawFilledRectangle ds r
      LongestStraightLine -> drawLongestStraightLine ds r

removeAllDrawnShapes = do
  dss :: DrawnShapes <- XS.get
  removeAllDrawnShapes' $ drawnShapes dss
  XS.put $ dss { drawnShapes = [] }
removeAllDrawnShapes' dss =
  destoryAllWindows' $ L.foldr (\ds wins -> wins ++ (L.map win $ drawnWindows ds)) [] $ dss

removeLatestDrawnShape = do
  dss :: DrawnShapes <- XS.get
  if L.null $ drawnShapes dss then
      return ()
  else do
      destoryAllWindows' $ L.map win $ drawnWindows $ head $ drawnShapes dss
      XS.put $ dss { drawnShapes = tail $ drawnShapes dss }

redrawAllDrawnShapes raise = do
  dss :: DrawnShapes <- XS.get
  redrawAllDrawnShapes' raise $ drawnShapes dss
redrawAllDrawnShapes' raise dss = do
  L.foldr (\ds x -> do
               x
               redrawShape raise ds
          ) (return ()) $ dss
redrawShape raise ds@DrawnShape {drawnWindows = wins, drawnShape = s} = withDisplay $ \dpy -> do
  L.foldr (\dw x -> do
             x
             if raise then liftIO $ raiseWindow dpy $ win dw else return ()
             drawInside dpy (win dw) (rect_width $ winRect dw) (rect_height $ winRect dw) $ color s
          ) (return ()) wins
drawShapeEventHook e = do
  case e of
    (MapNotifyEvent ev_event_type ev_serial ev_send_event ev_event_display ev_parent ev_window ev_override_redirect) -> redrawAllDrawnShapes True
    _ -> redrawAllDrawnShapes False
  return $ All True

drawOutlinedRectangle ds@DrawShape{
                            width = width'
                      }
                      rect@Rectangle{
                                rect_x = x,
                                rect_y = y,
                                rect_width = width,
                                rect_height = height
                      } = do
  let w = fromIntegral width'
  win1 <- drawVerLine ds x y (fromIntegral height) w
  win2 <- drawHorLine ds x y (fromIntegral width) w
  win3 <- drawVerLine ds (x+(fromIntegral width)) y (fromIntegral height) w
  win4 <- drawHorLine ds x (y+(fromIntegral height)) (fromIntegral width + w) w
  return $ DrawnShape {
               drawnWindows = [win1, win2, win3, win4],
               drawnShape = ds
           }

drawFilledRectangle ds -- ignore width
                    rect@Rectangle{
                              rect_x = x,
                              rect_y = y,
                              rect_width = width,
                              rect_height = height
                    } = do
  win <- drawLine' ds x y width height
  return $ DrawnShape {
               drawnWindows = [win],
               drawnShape = ds
           }

drawLongestStraightLine ds@DrawShape{
                       width = width'
                 }
                 rect@Rectangle{
                           rect_x = x,
                           rect_y = y,
                           rect_width = width,
                           rect_height = height
                 } = do
  win <- if width < height then
             drawVerLine ds x y height w
         else
             drawHorLine ds x y width w
  return $ DrawnShape {
               drawnWindows = [win],
               drawnShape = ds
           }
  where w = fromIntegral width'

drawVerLine ds x y len width = drawLine' ds x y width len

drawHorLine ds x y len width = drawLine' ds x y len width

drawLine' ds x y width height = withDisplay $ \dpy -> do
  rootw <- asks theRoot
  win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw x y width height
  liftIO $ mapWindow dpy win
  drawInside dpy win width height $ color ds
  return $ DrawnWindow win $ Rectangle x y width height

drawInside dpy win width height rgb = do
  gc <- liftIO $ createGC dpy win
  liftIO $ setRGBForeground dpy gc rgb
  liftIO $ fillRectangle dpy win gc (fromInteger 0) (fromInteger 0) (fromIntegral width) height
  liftIO $ freeGC dpy gc

destoryAllWindows' wins = withDisplay $ \dpy -> do
  liftIO $ L.foldr (\w io -> destroyWindow dpy w >> io) (return ()) wins

setRGBForeground dpy gc rgb = do
  setForeground dpy gc $ rgbColorPointer rgb
rgbColorPointer rgb@RGB { rgb_red = r, rgb_green = g, rgb_blue = b } = (fromIntegral b) + (shiftL (fromIntegral g) 8) + (shiftL (fromIntegral r) 16)

grabCursorForDraw dpy win = do
  cur <- createFontCursor dpy xC_crosshair
  let mask = buttonPressMask .|. buttonReleaseMask .|. pointerMotionMask
  _ <- grabPointer dpy win False mask grabModeAsync grabModeAsync none cur currentTime
  return cur

ungrabCursorForDraw dpy cur = do
  ungrabPointer dpy currentTime
  freeCursor dpy cur

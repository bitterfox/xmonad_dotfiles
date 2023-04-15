
module XMonad.Util.DocksSupport (
  docksStartupHook,
  docksManageHook,
  docksOnBottom
) where

import qualified Data.List as L

import Control.Monad (filterM)

import XMonad
import qualified XMonad.Hooks.ManageDocks as MD
import XMonad.Util.WindowProperties (getProp32s)

------------------------------------------------------------------------------------------
-- Support for docks
-- Desktop lowest, Docks higher than desktop but lower than other windows
------------------------------------------------------------------------------------------
docksStartupHook = MD.docksStartupHook <+> docksOnBottom

docksManageHook = MD.manageDocks <+> (MD.checkDock --> (liftX docksOnBottom >> mempty))

docksOnBottom = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    (_,_,wins) <- io $ queryTree dpy rootw
    docks <- filterM (runQuery checkDockOnly) wins
    desks <- filterM (runQuery $ checkDesktopOnly <&&> checkBackgroundDesktop) wins
--    forM (docks ++ desks) $ \win -> do
--      name <- runQuery className win
--      withWindowAttributes dpy win $ \(WindowAttributes {wa_x = x, wa_y = y, wa_width = w, wa_height = h}) -> do
--        (_, p, cs) <- io $ queryTree dpy win
--        let s = (show x) ++ "," ++ (show y) ++ "," ++ (show w) ++ "," ++ (show h)
--        pn <- runQuery className p
--        spawn $ "echo '" ++ ((show p) ++ "," ++ (show pn)) ++ " -> " ++ (show win) ++ ":" ++ (show name) ++ "," ++ s ++ (" -> " ++ (show cs)) ++ "' >> /tmp/xmonad.debug.docks"
    io $ L.foldr (>>) (return ()) $ L.map (lowerWindow dpy) $ L.reverse docks
    io $ L.foldr (>>) (return ()) $ L.map (lowerWindow dpy) $ L.reverse desks

checkDockOnly = ask >>= \w -> liftX $ do
  dock <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (== dock) (map fromIntegral rs)
    _       -> return False
checkDesktopOnly = ask >>= \w -> liftX $ do
  desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (== desk) (map fromIntegral rs)
    _       -> return False
checkBackgroundDesktop = className =? "Nautilus"
------------------------------------------------------------------------------------------
-- Support for docks
------------------------------------------------------------------------------------------

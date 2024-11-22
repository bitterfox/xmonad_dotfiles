{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module XMonad.Layout.WindowViewableLayout (
  WindowViewableLayout(..),
  WindowViewState(..),
  WindowViewMessage(..),
  startWindowView, quitWindowView,
) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

data WindowViewableLayout windowViewLayout layout a = WindowViewableLayout (WindowViewState) (windowViewLayout a) (layout a) deriving ( Read, Show )

data WindowViewState = Normal | WindowView deriving ( Read, Show, Typeable )

data WindowViewMessage = View | Focus deriving ( Typeable )
instance Message WindowViewMessage

instance (LayoutClass l1 a, LayoutClass l2 a) => LayoutClass (WindowViewableLayout l1 l2) a where
    runLayout (W.Workspace i l@(WindowViewableLayout Normal l1' l2') ms) r = do
      (ws, ml) <- runLayout (W.Workspace i l2' ms) r
      case ml of
        Just nl -> return (ws, Just (WindowViewableLayout Normal l1' nl))
        Nothing -> return (ws, Nothing)

    runLayout (W.Workspace i l@(WindowViewableLayout WindowView l1' l2') ms) r = do
      (ws, ml) <- runLayout (W.Workspace i l1' ms) r
      case ml of
        Just nl -> return (ws, Just (WindowViewableLayout WindowView nl l2'))
        Nothing -> return (ws, Nothing)

    handleMessage l@(WindowViewableLayout state l1 l2) mess = do
      case fromMessage mess of
        Just View -> do
            handleMessage l2 $ SomeMessage Hide
            return $ Just $ WindowViewableLayout WindowView l1 l2
        Just Focus -> do
            handleMessage l1 $ SomeMessage Hide
            return $ Just $ WindowViewableLayout Normal l1 l2
        other -> delegateHandleMessage l mess

    description (WindowViewableLayout state l1 l2) = description l2
--      case state of
--        Normal -> "Normal" ++ (description l2)
--        WindowView -> "WindowView" ++ (description l1)

delegateHandleMessage (WindowViewableLayout Normal l1 l2) mess = do
  ml <- handleMessage l2 mess
  case ml of
    Just nl -> return $ Just (WindowViewableLayout Normal l1 nl)
    Nothing -> return Nothing
delegateHandleMessage (WindowViewableLayout WindowView l1 l2) mess = do
  ml <- handleMessage l1 mess
  case ml of
    Just nl -> return $ Just (WindowViewableLayout WindowView nl l2)
    Nothing -> return Nothing

instance ExtensionClass WindowViewState where
  initialValue = Normal

startWindowView = do
  broadcastMessage View
  XS.put WindowView
  refresh

quitWindowView whenNormal = do
  windowViewState <- XS.get
  XS.put Normal
  case windowViewState of
    WindowView -> broadcastMessage Focus >> refresh
    Normal -> whenNormal

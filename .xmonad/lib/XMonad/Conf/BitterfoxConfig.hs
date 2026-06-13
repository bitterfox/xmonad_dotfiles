module XMonad.Conf.BitterfoxConfig where

import XMonad
import XMonad.Core

import XMonad.Util.Performance

import XMonad.Conf.MyConf
import XMonad.Conf.MyParameters
import XMonad.Conf.MyHook
import XMonad.Conf.MyKeys
import XMonad.Conf.MyMouse
import XMonad.Conf.MyXMobar

bitterfoxConfig xmprocs = do
  let config = configureKeys $ baseConfig {
      startupHook = myStartupHook <+> describeMyKeys
    , manageHook = myManageHookAll
    , layoutHook =  myLayoutHookAll
    , logHook = measure "logHook" $ myLogHook $ xmobarLogHook xmprocs
    , handleEventHook = \e -> measure "handleEventHook" $ myHandleEventHook e
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , borderWidth = 4
    , normalBorderColor  = blue
    , focusedBorderColor = red
    , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
    , clickJustFocuses = False
    , XMonad.Core.workspaces = myWorkspaces
  }
  configureMouse config

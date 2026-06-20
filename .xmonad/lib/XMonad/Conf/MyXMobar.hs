{-# OPTIONS_GHC -Wno-deprecations #-}
module XMonad.Conf.MyXMobar where

import System.IO
import System.Directory
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run (spawnPipe, runProcessWithInput)
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedWindows

import XMonad.Util.NamedScratchpad2
import XMonad.Util.PhysicalScreen
import XMonad.Util.VirtualScreen
import XMonad.Util.WorkspaceFamily

import XMonad.Conf.MyParameters
import XMonad.Conf.MyConf

startXMobar :: IO [Handle]
startXMobar = do
  homeDirectory <- liftIO getHomeDirectory
  numDisplayStr <- runProcessWithInput "sh" ["-c", "xrandr --query | grep -c '\\bconnected\\b'"] ""
  let numDisplay = read numDisplayStr :: Int
  dpiStr <- runProcessWithInput "sh" ["-c", "xrdb -query | grep Xft.dpi | awk '{print $2}'"] ""
  let dpi = read dpiStr :: Int
  spawn $ "echo '" ++ (show numDisplay) ++ "' > /tmp/test"
  spawn $ "xrandr --query | grep -c '\\bconnected\\b' >> /tmp/test"
  xmprocs <- mapM (\displayId -> spawnPipe $ "export FONTCONFIG_FILE=" ++ homeDirectory ++ "/.xmobar/font.conf && /usr/bin/xmobar -D " ++ (show dpi) ++ " " ++ (if displayId == 0 then "" else "-p 'TopSize L 100 30' -x " ++ (show displayId)) ++ " ~/.xmobarrc") [0..numDisplay-1]
  return xmprocs

xmobarLogHook xmprocs = withWindowSet (\s ->
    L.foldl (>>) def (map (\(i, xmproc) -> do
--      originalScreenIdToCurrentScreenIdMap <- originalScreenIdToCurrentScreenId priorityDisplayEDIDs
        j <- (\(OriginalDisplayIdToCurrentScreenId idToId) -> fromMaybe i $ M.lookup i idToId) <$> XS.get
        vss <- getVirtualScreens
        let screenList = toScreenList s vss j
        let virtualScreenList = toVirtualScreenList s vss j
        let header = if virtualScreenList == "" then screenList else screenList ++ " | " ++ virtualScreenList
        let S virtualScreenId = maybe (S j) (\vs -> W.focus $ screenStack vs) $ findVirtualScreen vss $ S j
        dynamicLogWithPP (multiScreenXMobarPP s virtualScreenId header xmproc)) (L.zip [0..(L.length xmprocs)] xmprocs)))

toScreenList ws vss xmobarScreenId = do
  let sidCurrentActive = W.screen $ W.current ws
  let S sidCurrentActiveRoot = maybe sidCurrentActive (\vs -> rootSid vs) (findVirtualScreen vss sidCurrentActive)
  let sids = L.sort $ L.map (\(S sid) -> sid) $ L.map W.screen $ W.screens ws
  let screenDesc = L.map (\sid -> do
                            let screenStr = toScreen sid sidCurrentActiveRoot xmobarScreenId
                            let vsMaybe = findVirtualScreen vss $ S sid
                            maybe (screenStr) (\vs -> if rootSid vs == S sid then screenStr else "") vsMaybe
                         ) sids
  L.foldr (++) ("") screenDesc

toScreen sid currentActive xmobarScreenId =
    if xmobarScreenId == sid then
        if sid == currentActive then
            xmobarColor'
            (wrap " " " " $ show $ sid + 1)
            white red True
        else
            xmobarColor'
            (wrap " " " " $ show $ sid + 1)
            blue white False
    else
        xmobarColor'
        (wrap " " " " $ show $ sid + 1)
        black white $ sid == currentActive

toVirtualScreenList ws vss xmobarScreenId = do
  let S sidCurrentActive = W.screen $ W.current ws
  case findVirtualScreen vss $ S xmobarScreenId of
    Just vs -> do
      let sids = L.map (\(S sid) -> sid) $ W.integrate $ screenStack vs
      L.foldr (++) ("") $ L.map (toVirtualScreen sidCurrentActive) sids
    Nothing -> ""
toVirtualScreen currentActive sid =
    xmobarColor'
    (wrap " " " " $ show $ sid + 1)
    black white $ sid == currentActive

multiScreenXMobarPP windowSet screenId header xmproc = xmobarPP
                        { ppOutput = \t -> hPutStrLn xmproc $ header ++ " | " ++ (fallbackIfNoScreen (\ws -> \sid -> \fid -> fid) windowSet screenId) ++ " | " ++ t ++ "  |  "
                        , ppTitle = \t -> ""
                        , ppSep             = " | "
                        , ppExtras = [ titleOfScreenId windowSet screenId ]
                        , ppCurrent = fallbackIfNoScreen (showOnlyWorkspaceFor $ currentOfScreenId False) windowSet screenId
                        , ppVisible = fallbackIfNoScreen visibleOfScreenId windowSet screenId
                        , ppHidden = fallbackIfNoScreen (showOnlyWorkspaceFor $ \ws -> \sid -> ppHidden xmobarPP) windowSet screenId
                        , ppLayout = \t -> layoutOfScreenId windowSet screenId
                        , ppSort = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP
                        }

xmobarColor' text foreground background bool =
    xmobarColor (if bool then foreground else background) (if bool then background else foreground) text

titleOfScreenId windowSet screenId =
    case L.find (\sc -> W.screen sc == S screenId) $ W.screens windowSet of
      Just sc -> case (W.stack $ W.workspace sc) >>= (\st -> Just $ W.focus st) of
                   Just w -> do
                       name <- getName w
                       return $ Just $ xmobarColor' (wrap " " (replicate 300 ' ') $ show name) black white $ (W.screen $ W.current windowSet) == W.screen sc
                   Nothing -> def
      Nothing -> titleOfScreenId windowSet 0 -- optimize

layoutOfScreenId windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> do
        let layout = W.layout $ W.workspace sc
        description layout
--        case layout of
--          Layout l -> case l of
--                        ResizableTall _nmaster _delta _frac _slaves -> "yeah"
--                        _ -> description layout
--          _ -> description layout
      Nothing -> layoutOfScreenId windowSet 0 -- optimize

currentOfScreenId spaceForOther windowSet screenId =
    if W.screen(W.current windowSet) == S screenId then
        xmobarColor black white . wrap " " " "
    else if spaceForOther then
             wrap " " " "
         else
             wrap "" ""

visibleOfScreenId windowSet screenId familyId tag =
    showOnlyWorkspaceFor (\ws -> \sid -> \wid ->
                                 case L.find (\sc -> (W.screen sc) == S screenId) (W.visible windowSet) of
                                   Just sc -> if (W.tag $ W.workspace sc) == tag then xmobarColor black white $ wrap " " " " wid else wrap "" "" wid
                                   Nothing -> wrap "" "" wid) windowSet screenId familyId tag

showOnlyWorkspaceFor f windowSet screenId fid = \w ->
                                  case readWorkspaceInFamily w of
                                    Just wif -> if familyId wif == fid then
                                                    f windowSet screenId $ workspaceId wif
                                                else ""
                                    Nothing -> f windowSet screenId w

screenIds windowSet xmobarScreenId =
    L.foldr (\a -> \b -> a ++ b) "" $ L.map
         (\sid ->
              xmobarColor'
              (wrap (if xmobarScreenId == sid then "[" else " ") (if xmobarScreenId == sid then "]" else " ") $
                    show $ sid + 1)
              black white $
                        (fromIntegral sid) == (W.screen $ W.current windowSet)) [0 .. L.length $ W.visible windowSet]

fallbackIfNoScreen f windowSet screenId =
  let (sid, tag) = case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
                     Just sc -> (screenId, W.tag $ W.workspace $ sc)
                     Nothing -> (0, W.tag $ W.workspace $ W.current windowSet)
  in f windowSet sid $ (fromMaybe tag $ toFamilyIdMaybe tag)

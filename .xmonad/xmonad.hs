{-# LANGUAGE DeriveDataTypeable #-}
import XMonad
import XMonad.Core
import XMonad.Actions.CycleWS
import XMonad.Actions.Volume
import XMonad.Actions.GridSelect
import XMonad.Actions.Search (selectSearchBrowser, google)
--import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import qualified XMonad.StackSet as W
import XMonad.Layout.ToggleLayouts
import XMonad.ManageHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import qualified XMonad.Util.ExtensibleState as XS
import System.Exit
import System.IO
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Control.Monad (foldM, filterM, mapM)
import Control.Concurrent
import qualified Text.Show as TS

onTop = (customFloating $ W.RationalRect 0 0.02 1 0.48)

java9Home = "~/bin/jdk9b164"
jshellPath = java9Home ++ "/bin/jshell"

terminalScratchpad :: String -> Maybe String -> ManageHook -> NamedScratchpad
terminalScratchpad name execMaybe manageHook =
    NS name
       ("/usr/lib/gnome-terminal/gnome-terminal-server" ++
           " --app-id bitter_fox.xmonad." ++ name ++
           " --name=" ++ name ++ " --class=" ++ name ++
           " & gnome-terminal --app-id bitter_fox.xmonad." ++ name ++
           (case execMaybe of
              Just exec -> " -e " ++ exec
              Nothing -> ""
           )
       )
       (appName =? name)
       manageHook

myScratchpads :: [NamedScratchpad]
myScratchpads = [
    terminalScratchpad "mainterm" Nothing
           (customFloating $ W.RationalRect 0 0.02 1 0.98)
  , terminalScratchpad "term1" Nothing
           (customFloating $ W.RationalRect 0 0.02 1 0.48)
  , terminalScratchpad "term2" Nothing
           (customFloating $ W.RationalRect 0 0.5 1 0.5)
  , terminalScratchpad "jshell1"
           (Just jshellPath)
           (customFloating $ W.RationalRect 0 0.02 1 0.48)
  , terminalScratchpad "jshell1"
           (Just jshellPath)
           (customFloating $ W.RationalRect 0 0.5 1 0.5)
  , NS "bunnaru"
           "google-chrome --renderer-process-limit=1 --new-window --app=http://www.dmm.com/netgame/social/-/gadgets/=/app_id=798209/"
           (appName =? "www.dmm.com__netgame_social_-_gadgets_=_app_id=798209")
           (customFloating $ W.RationalRect 0 0.4 0.55 0.6)
  , NS "艦これ"
           "google-chrome --renderer-process-limit=1 --new-window --app=http://www.dmm.com/netgame/social/-/gadgets/=/app_id=854854/"
           (appName =? "www.dmm.com__netgame_social_-_gadgets_=_app_id=854854")
           (customFloating $ W.RationalRect 0.55 0.4 0.45 0.6)
 ]


myScratchpadsManageHook = namedScratchpadManageHook myScratchpads

myNamedScratchpadAction n =
    namedScratchpadAction myScratchpads n
    >> withWindowSet (\s ->
         case (W.peek s, L.find isSameName myScratchpads) of
           (Just w, Just ns) -> do
             hook <- runQuery (hook ns) w
             isTarget <- runQuery (query ns) w
             case isTarget of
               True -> windows(\ws -> appEndo hook ws)
               _ -> return ()
           _ -> return ()
       )
  where
    isSameName = \ns -> n == (name ns)

myNamedScratchpadActionMaybe mns = case mns of
    Just ns -> myNamedScratchpadAction $ name ns
    _ -> return ()

toggleScrachpadAction scratchpads =
    withWindowSet (\s ->
      case W.peek s of
        Just w -> do
          (cur, next) <- findCurrentAndNextScrachpadOf scratchpads w
          myNamedScratchpadActionMaybe cur >> myNamedScratchpadActionMaybe next
        _ -> myNamedScratchpadAction $ name (head scratchpads)
    )

findCurrentAndNextScrachpadOf nss w =
    findIt (cycle ((asMaybe nss) ++ [Nothing]))
  where
    asMaybe = \l -> map Just l
    findIt = \l ->
               let
                   h = head l
                   t = tail l
               in
                 case h of
                   Just ns -> do
                          matched <- runQuery (query ns) w
                          if matched then
                              return (h, head t)
                          else
                              findIt t
                   _ -> return (h, head t)


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
    acceleration = 0.0001
    delay = 10
    toShowActions = \s ws -> do
                      MaybeWindowLocationMap evacuateds <- XS.get
                      let
                        currents = W.floating s;
                        toBeShowns = mapMaybe (\w -> do
                          current <- M.lookup w currents
                          (W.RationalRect x y width height, (_x, _y))  <- M.lookup w evacuateds
                          Just (w, current, (x, y))
                          ) ws
                      io $ appendFile "/tmp/xmonad.debug" "show\n"
                      io $ appendFile "/tmp/xmonad.debug" $ TS.show toBeShowns
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
                      io $ appendFile "/tmp/xmonad.debug" "hide\n"
                      io $ appendFile "/tmp/xmonad.debug" $ TS.show toBeEvacuateds
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
                   io (if delay == 0 then return () else threadDelay delay)
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

notSP :: X (WindowSpace -> Bool)
notSP = return $ ("NSP" /=) . W.tag

nextWS' :: X ()
nextWS' = moveTo Next (WSIs notSP)
prevWS' :: X ()
prevWS' = moveTo Prev (WSIs notSP)

shiftToNextWS' :: X()
shiftToNextWS' = shiftTo Next (WSIs notSP)
shiftToPrevWS' :: X()
shiftToPrevWS' = shiftTo Prev (WSIs notSP)

tall = Tall 1 (3/100) (1/2)

watch :: String -> String -> IO ()
watch cmd interval = spawn $ "while :; do " ++ cmd ++ "; sleep " ++ interval ++ "; done"

main = do
--    spawn "unity-settings-daemon" -- Unity上での設定を反映させる
    spawn "systemd --user"
    spawn "gnome-session"
    io (threadDelay (1 * 1000 * 1000)) -- Wait unity-settings-daemon reflect their settings

--    watch "xmodmap ~/.xmodmap" "0.3"

--    spawn "ginn .wishes.xml" -- for Mac mouse

    spawn "nautilus-desktop --force" -- デスクトップを読み込む

--    spawn "killall trayer ; sleep 2 ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --widthtype percent --transparent false --tint 0x000000 --height 22" -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus

--    spawn "gnome-power-manager"
--    spawn "killall nm-applet ; nm-applet" -- ネット接続のアプレットを起動
--    spawn "gnome-sound-applet" -- gnome-volume-control-applet? -- ボリューム変更のアプレットを起動
--    spawn "bluetooth-applet"
--    spawn "sparkleshare restart"
--  spawn "/opt/toggldesktop/TogglDesktop.sh"

    -- gnome-sound-appletのアイコンが黒一色でない場合は--transparent trueにすると統一感があっていいです。 -- GNOMEのトレイを起動 -- XXX(sleep 2): #6: Trayer broken with nautilus
--    spawn "sleep 10 ; killall trayer ; trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --widthtype percent --transparent false --tint 0x000000 --height 22 ; dropbox start"
    spawn "trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --width 10 --widthtype percent --transparent false --tint 0x000000 --height 22 ; dropbox start"
    -- dropboxを起動させて同期できるようにする

    spawn "wmname LG3D"
--    spawn "prlcc"
--    spawn "prlcp"

--    spawn "compton -b --config ~/.comptonrc"
--    spawn "sleep 5; gnome-session"

    xmproc0 <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmproc1 <- spawnPipe "/usr/bin/xmobar -x 1 ~/.xmobarrc"
    let xmprocs = [xmproc0, xmproc1]
    xmonad $ gnomeConfig -- defaultConfig
        { manageHook = manageHook gnomeConfig -- defaultConfig
                       <+> manageDocks
                       <+> namedScratchpadManageHook myScratchpads
        , layoutHook = avoidStruts $
                       toggleLayouts (renamed [Replace "□"] $ noBorders Full)
 $                       ((renamed [Replace "├"] $ myLayout) ||| (renamed [Replace "┬"] $ Mirror myLayout)) -- tall, Mirror tallからFullにトグルできるようにする。(M-<Sapce>での変更はtall, Mirror tall)
        , logHook = withWindowSet (\s -> L.foldl (>>) def (map (\(i, xmproc) -> dynamicLogWithPP (multiScreenXMobarPP s i xmproc)) (L.zip [0..(L.length xmprocs)] xmprocs)))
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , borderWidth = 6
        , normalBorderColor  = "#587993" -- Java blue
        , focusedBorderColor = "#e76f00" -- Java orange
        , focusFollowsMouse = False -- マウスの移動でフォーカスが映らないように
        , clickJustFocuses = False
        } `additionalKeys`
        [
          ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock") -- Lock
        , ((mod4Mask .|. shiftMask, xK_s), spawn "gnome-screensaver-command --lock ; dbus-send --print-reply --system --dest=org.freedesktop.UPower /org/freedesktop/UPower org.freedesktop.UPower.Suspend") -- Lock & Suspend
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_l), io (exitWith ExitSuccess)) -- Logout
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_s), spawn "/usr/lib/indicator-session/gtk-logout-helper --shutdown") -- Shutdown

        , ((controlMask, xK_Print), spawn "gnome-screenshot -c")
        , ((0, xK_Print), spawn "gnome-screenshot")
        , ((mod4Mask, xK_r), refresh)

        , ((mod4Mask .|. shiftMask, xK_e), spawn "nautilus")

        , ((mod4Mask, xK_Return), myNamedScratchpadAction "mainterm")
        , ((mod4Mask, xK_F9), myNamedScratchpadAction "艦これ")
        , ((mod4Mask, xK_F10), myNamedScratchpadAction "bunnaru")
        , ((mod4Mask, xK_F11), myNamedScratchpadAction "term1")
        , ((mod4Mask, xK_F12), myNamedScratchpadAction "term2")
        , ((mod4Mask .|. controlMask, xK_F11), myNamedScratchpadAction "jshell1")
        , ((mod4Mask .|. controlMask, xK_F12), myNamedScratchpadAction "jshell2")

        , ((mod4Mask .|. controlMask, xK_F7), toggleScrachpadAction $ L.reverse myScratchpads)
        , ((mod4Mask .|. controlMask, xK_F8), showOrHideScratchpads myScratchpads True)
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_F8), showOrHideScratchpads myScratchpads False)
        , ((mod4Mask .|. controlMask, xK_F9), toggleScrachpadAction myScratchpads)

        , ((mod4Mask, xK_g), selectSearchBrowser "/usr/bin/google-chrome-stable" google)

        , ((mod4Mask, xK_d), sendMessage NextLayout)
        -- Full screen
        , ((mod4Mask, xK_f), sendMessage ToggleLayout)

        -- 水平のサイズ変更
        , ((mod4Mask, xK_i), sendMessage MirrorExpand)
        , ((mod4Mask, xK_m), sendMessage MirrorShrink)

        -- Arrow Keys
        -- フォーカスの移動
        , ((mod4Mask, xK_Up), windows W.focusUp)
        , ((mod4Mask, xK_Left), prevWS')
        , ((mod4Mask, xK_Down), windows W.focusDown)
        , ((mod4Mask, xK_Right), nextWS')

        -- スワップ
        , ((mod4Mask .|. shiftMask, xK_Up), windows W.swapUp)
        , ((mod4Mask .|. shiftMask, xK_Left), windows W.swapUp)
        , ((mod4Mask .|. shiftMask, xK_Down), windows W.swapDown)
        , ((mod4Mask .|. shiftMask, xK_Right), windows W.swapDown)

        -- ワーススペースの移動
        , ((mod4Mask .|. controlMask, xK_Up), prevWS')
        , ((mod4Mask .|. controlMask, xK_Left), prevWS')
        , ((mod4Mask .|. controlMask, xK_Down), nextWS')
        , ((mod4Mask .|. controlMask, xK_Right), nextWS')

        -- ワーススペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up), shiftToPrevWS' >> prevWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Left), shiftToPrevWS' >> prevWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down), shiftToNextWS' >> nextWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_Right), shiftToNextWS' >> nextWS')

        -- j/k key
        -- ワークスペースの移動
        , ((mod4Mask .|. controlMask, xK_j), nextWS')
        , ((mod4Mask .|. controlMask, xK_k), prevWS')

        -- ワークスペースの移動
        , ((mod4Mask .|. mod1Mask, xK_j), nextScreen)
        , ((mod4Mask .|. mod1Mask, xK_k), prevScreen)

        , ((mod4Mask, xK_space), nextScreen)
        , ((mod4Mask .|. shiftMask, xK_space), prevScreen)

        -- ワークスペース間のスワップ
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_j), shiftToNextWS' >> nextWS')
        , ((mod4Mask .|. controlMask .|. shiftMask, xK_k), shiftToPrevWS' >> prevWS')

        , ((mod4Mask, xK_w), goToSelected hidpiGSConfig)
        , ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace hidpiGSConfig W.view)

        , ((mod4Mask, xK_e), spawnSelected hidpiGSConfig applications)
        , ((mod4Mask, xK_s), scratchpadSelected hidpiGSConfig myScratchpads)
        ] `additionalKeysP`
        [
        -- ボリューム周り
          ("<XF86AudioLowerVolume>", setMute(False) >> lowerVolume 3 >> return ())
        , ("<XF86AudioRaiseVolume>", setMute(False) >> raiseVolume 3 >> return ())
        , ("<XF86AudioMute>",        setMute(False) >> setVolume 50   >> return ()) -- toggleMuteで問題がなければそうすると良いです。
        ] `removeKeys`
        [
          (mod4Mask .|. shiftMask, xK_q)
        ] `additionalMouseBindings`
        [
          ((mod4Mask .|. controlMask, button1), \w -> focus w >> mouseResizeWindow w
                                                              >> windows W.shiftMaster)
        ]

myLayout = (ResizableTall 1 (3/100) (1/2) [])

hidpiGSConfig :: HasColorizer a => GSConfig a
hidpiGSConfig = defaultGSConfig { gs_cellheight = 80, gs_cellwidth = 800, gs_font = "xft:Sans-9" }

applications = [
 "google-chrome",
 "nautilus",
 "emacs",
 "gnome-terminal",
 "unity-control-center",
 "libreoffice",
 "~/bin/idea-IU-181.5087.20/bin/idea.sh",
 "/usr/local/pulse/pulseUi"]

scratchpadSelected :: GSConfig NamedScratchpad -> [NamedScratchpad] -> X()
scratchpadSelected config scratchpads = do
    scratchpadMaybe <- gridselect config (map (\s -> (name s, s)) scratchpads)
    myNamedScratchpadActionMaybe scratchpadMaybe

multiScreenXMobarPP windowSet screenId xmproc = xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = \t -> ""
                        , ppExtras = [ titleOfScreenId windowSet screenId ]
                        , ppCurrent = fallbackIfNoScreen currentOfScreenId windowSet screenId
                        , ppVisible = fallbackIfNoScreen visibleOfScreenId windowSet screenId
                        , ppLayout = \t -> fallbackIfNoScreen layoutOfScreenId windowSet screenId
                        , ppSort = fmap (. namedScratchpadFilterOutWorkspace) $ ppSort xmobarPP
                        }

titleOfScreenId windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> case ((W.stack (W.workspace sc)) >>= (\st -> Just (W.focus st))) of
                   Just w -> fmap (\nw -> Just (xmobarColor (if ((W.screen (W.current windowSet)) == (W.screen sc)) then "green" else "gray") "" (show nw))) (getName w)
                   Nothing -> def
      Nothing -> titleOfScreenId windowSet 0 -- optimize

layoutOfScreenId windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> description . W.layout . W.workspace $ sc
      Nothing -> layoutOfScreenId windowSet 0 -- optimize

currentOfScreenId windowSet screenId = if (W.screen(W.current windowSet) == S screenId) then xmobarColor "yellow" "" . wrap "[" "]" else wrap "" ""

visibleOfScreenId windowSet screenId wid =
    case L.find (\sc -> (W.screen sc) == S screenId) (W.visible windowSet) of
      Just sc -> if (W.tag (W.workspace sc) == wid) then wrap "[" "]" wid else wrap "" "" wid
      Nothing -> wrap "" "" wid

fallbackIfNoScreen f windowSet screenId =
    case (L.find (\sc -> (W.screen sc) == S screenId) (W.screens windowSet)) of
      Just sc -> f windowSet screenId
      Nothing -> f windowSet 0



module XMonad.Actions.IntelliJTerminal (
  launchIntelliJTerminal,
  intelliJTerminalQuery,
  intelliJTerminalManageHook,
  IntelliJTerminalEnvironment(..)
) where

import qualified Data.List as L
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.String (Parser)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS))
import XMonad.Util.MyNamedScratchpad

------------------------------------------------------------------------------------------
-- IntelliJExternalTerminal
------------------------------------------------------------------------------------------

data IntelliJTerminalEnvironment = IntelliJTerminalEnvironment {
  homeDirectory :: String,
  hook :: ManageHook
}

intelliJTerminalManageHook env = intelliJTerminalQuery --> hook env

intelliJTerminalAppId name = "xmonad.intellij." ++ name

intelliJTerminalQuery = appName >>= return . L.isPrefixOf "xmonad.intellij."

intelliJScrachpad :: String -> String -> ManageHook -> NamedScratchpad
intelliJScrachpad name workingDir manageHook =
    NS name
       ("/usr/lib/gnome-terminal/gnome-terminal-server" ++
           " --app-id " ++ appId ++
           " --name=" ++ appId ++ " --class=" ++ "intellij-terminal" ++
           " & gnome-terminal --app-id " ++ appId ++
           " --working-directory=" ++ workingDir
       )
       (appName =? ("xmonad.intellij." ++ name))
       manageHook
  where appId = intelliJTerminalAppId name

launchIntelliJTerminal :: IntelliJTerminalEnvironment -> X ()
launchIntelliJTerminal env =
    withFocused (\w -> do
      cname <- runQuery className w
      if cname == "jetbrains-idea" then do
          t <- runQuery title w
          case (parse intelliJInfo "/tmp/hoge" t) of
            Left e -> return ()
            Right [project, dir] -> do
                   let name = project ++ (T.unpack $ T.replace (T.pack "~") (T.pack "") $ T.replace (T.pack "/") (T.pack ".") $ T.pack dir)
                   withWindowSet (\s -> L.foldr (>>) (return ()) $ L.map (hideIntelliJTerminal env name) $ W.integrate' $ W.stack $ W.workspace $ W.current s)
                   runScratchpadAction $ intelliJScrachpad name (extractHomeDirectory env dir) (hook env)
      else withWindowSet (\s -> L.foldr (>>) (return ()) $ L.map (hideIntelliJTerminal env "") $ W.integrate' $ W.stack $ W.workspace $ W.current s)
    )

hideIntelliJTerminal env exclude w = do
  aname <- runQuery appName w
  whenX (runQuery intelliJTerminalQuery w) $ do
    let name = L.drop (L.length "xmonad.intellij.") aname
    whenX (return $ name /= exclude) $ runScratchpadAction $ intelliJScrachpad name (homeDirectory env) (hook env)

intelliJInfo :: Parser [String]
intelliJInfo = do
  project <- many (noneOf [' '])
  char ' '
  char '['
  dir <- many (noneOf [']'])
  char ']'
  return [project, dir]

extractHomeDirectory env path =
    if L.head path == '~' then
        (homeDirectory env) ++ L.tail path
    else path
------------------------------------------------------------------------------------------
-- IntelliJExternalTerminal
------------------------------------------------------------------------------------------

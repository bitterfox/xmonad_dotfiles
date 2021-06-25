module XMonad.Actions.TerminalAction.GnomeBackend (
  GnomeTerminal(..)
) where

import qualified Data.List as L

import XMonad
import XMonad.Actions.TerminalAction
import qualified XMonad.Util.ExtensibleState as XS

data GnomeTerminal = GnomeTerminal {
      prefix :: String
}
data GnomeTerminalUniqueCount = GnomeTerminalUniqueCount Int deriving (Typeable)
instance ExtensionClass GnomeTerminalUniqueCount where
  initialValue = GnomeTerminalUniqueCount 0
instance Terminal GnomeTerminal where
--    terminalQuery (GnomeTerminal prefix) a = (appName =? (prefix ++ "." ++ (actionName a)))
    terminalQuery (GnomeTerminal prefix) a = (L.isPrefixOf $ prefix ++ "." ++ (actionName a)) <$> className
    startTerminal (GnomeTerminal prefix) (TerminalAction name _ script _ _) inFile outFile = do
      GnomeTerminalUniqueCount count <- XS.get
      XS.put $ GnomeTerminalUniqueCount $ count + 1
      let appId = prefix ++ "." ++ name ++ ".id" ++ (show count)
--      spawn $ "/usr/libexec/gnome-terminal/gnome-terminal-server" ++
--           " --app-id " ++ appId ++
--           " --name=" ++ appId ++ " --class=" ++ "xmonad-terminal" ++
--           " & gnome-terminal --app-id " ++ appId ++ " -- " ++ script ++ " " ++ inFile ++ " " ++ outFile
      spawn $ "gnome-terminal --class " ++ appId ++ " -- " ++ script ++ " " ++ inFile ++ " " ++ outFile

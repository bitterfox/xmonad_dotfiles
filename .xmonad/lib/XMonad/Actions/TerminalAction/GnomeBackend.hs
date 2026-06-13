module XMonad.Actions.TerminalAction.GnomeBackend (
  GnomeTerminal(..)
) where

import qualified Data.List as L

import XMonad
import XMonad.Actions.TerminalAction
import qualified XMonad.Util.ExtensibleState as XS

data GnomeTerminal = GnomeTerminal {
      prefix :: String,
      pathForServer :: String,
      pathForTerminal :: String,
      extraArgs :: String
}

instance Default GnomeTerminal where
    def = GnomeTerminal {
            prefix = "xmonad.terminal.action",
            pathForServer = "/usr/libexec/gnome-terminal-server",
            pathForTerminal = "/usr/bin/gnome-terminal",
            extraArgs = ""
    }

data GnomeTerminalUniqueCount = GnomeTerminalUniqueCount Int deriving (Typeable)
instance ExtensionClass GnomeTerminalUniqueCount where
  initialValue = GnomeTerminalUniqueCount 0
instance Terminal GnomeTerminal where
    terminalQuery term@(GnomeTerminal prefix _ _ _) a = (L.isPrefixOf $ prefix ++ "." ++ (actionName a)) <$> className
    startTerminal term@(GnomeTerminal prefix _ _ _) (TerminalAction name _ script _ _) inFile outFile = do
      GnomeTerminalUniqueCount count <- XS.get
      XS.put $ GnomeTerminalUniqueCount $ count + 1
      let appId = prefix ++ "." ++ name ++ ".id" ++ (show count)
      startTerminalInternal term appId $ Just $ script ++ " " ++ inFile ++ " " ++ outFile
    startTerminalCommand (GnomeTerminal _ pathForServer pathForTerminal extraArgs) appId command =
      pathForServer ++
           " --app-id " ++ appId ++
           " --name=" ++ appId ++ " --class=" ++ appId ++
           " & " ++ pathForTerminal ++ " --app-id " ++ appId ++
           (if null extraArgs then "" else " " ++ extraArgs) ++
           (case command of
              Just cmd -> " -- " ++ cmd ++ ""
              Nothing -> "")

module XMonad.Actions.TerminalAction.AlacrittyBackend (
  AlacrittyTerminal(..)
) where

import qualified Data.List as L

import XMonad
import XMonad.Actions.TerminalAction
import qualified XMonad.Util.ExtensibleState as XS

data AlacrittyTerminal = AlacrittyTerminal {
      prefix :: String,
      path :: String,
      extraArgs :: String
}

instance Default AlacrittyTerminal where
    def = AlacrittyTerminal {
            prefix = "xmonad.terminal.action",
            path = "/usr/bin/alacritty",
            extraArgs = ""
    }

data AlacrittyTerminalUniqueCount = AlacrittyTerminalUniqueCount Int deriving (Typeable)
instance ExtensionClass AlacrittyTerminalUniqueCount where
  initialValue = AlacrittyTerminalUniqueCount 0
instance Terminal AlacrittyTerminal where
    terminalQuery (AlacrittyTerminal prefix _ _) a = (L.isPrefixOf $ prefix ++ "." ++ (actionName a)) <$> className
    startTerminal term@(AlacrittyTerminal prefix path extraArgs) (TerminalAction name _ script _ _) inFile outFile = do
      AlacrittyTerminalUniqueCount count <- XS.get
      XS.put $ AlacrittyTerminalUniqueCount $ count + 1
      let appId = prefix ++ "." ++ name ++ ".id" ++ (show count)
      startTerminalInternal term appId $ Just $ script ++ " " ++ inFile ++ " " ++ outFile
    startTerminalCommand (AlacrittyTerminal _ path extraArgs) appId command =
      path ++
          " --class " ++ appId ++
          " " ++ extraArgs ++
          (case command of
             Just cmd -> " --command " ++ cmd
             Nothing -> "")

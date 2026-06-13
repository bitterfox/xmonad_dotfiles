module XMonad.Conf.MyParameters where

import XMonad.Layout.SimpleDecoration
import XMonad.Actions.DrawShape (RGB(..))
import XMonad.Util.PhysicalScreen (EDID(..))

------------------------------------------------------------------------------------------
-- Color
------------------------------------------------------------------------------------------
black = "#4E4B42"
brightBlack = "#635F54"
gray = "#B4AF9A"
darkWhite = "#CDC8B0"
white = "#DAD4BB"
red = "#CC654C"
redRGB = RGB { rgb_red = 204, rgb_green = 101, rgb_blue = 76 }
blue = "#3BA99F"
blueRGB = RGB { rgb_red = 59, rgb_green = 169, rgb_blue = 159 }

-- #C14BCC
purpleRGB = RGB { rgb_red = 193, rgb_green = 75, rgb_blue = 204 }
-- #4B6BCC
darkBlueRGB = RGB { rgb_red = 75, rgb_green = 107, rgb_blue = 204 }
-- #A89F3B
yellowRGB = RGB { rgb_red = 168, rgb_green = 159, rgb_blue = 59 }

------------------------------------------------------------------------------------------
-- Application
------------------------------------------------------------------------------------------
intellijCommand = "~/bin/idea"
applications = [
-- ("Vivaldi (Web browser)", "export GDK_DPI_SCALE=1.02; vivaldi"),
 ("Vivaldi (Web browser)", "vivaldi --remote-debugging-port=9222"),
 ("Nautilus (File browser)", "nautilus"),
 ("Emacs (Editor)", "emacs"),
 -- ("LINE", "wine '/home/jp21734/.wine/drive_c/users/jp21734/Local Settings/Application Data/LINE/bin/LineLauncher.exe'"),
 ("LINE", "vivaldi-stable --app='chrome-extension://ophjlpahpchlmihnnnihgmmeilfjmjjc/index.html#/'"),
 ("Configuration", "XDG_CURRENT_DESKTOP=GNOME gnome-control-center"),
 ("LibreOffice", "libreoffice"),
 ("JetBrains ToolBox", "~/bin/jetbrains-toolbox-1.14.5179/jetbrains-toolbox"),
 ("IntelliJ Idea", intellijCommand),
 ("PulseSecure", "/opt/pulsesecure/bin/pulseUI"),
 ("Steam", "steam"),
 ("Slack", "slack"),
 ("Discord", "discord"),
 ("Tweetdeck", webApplication "https://tweetdeck.twitter.com/"),
 ("YouTube", webApplication "https://youtube.com/"),
 ("DAZN", webApplication "https://dazn.com/")]
webApplication url = "vivaldi-stable --app=" ++ url
javaHome = "~/.sdkman/candidates/java/current"
jshellPath = javaHome ++ "/bin/jshell"

------------------------------------------------------------------------------------------
-- Display
------------------------------------------------------------------------------------------
priorityDisplayEDIDs :: [EDID]
priorityDisplayEDIDs = [
  -- "3840x2160+2294+0",
  -- "2294x1432+0+0",
  -- "1890x3360+6134+0",

  "00ffffffffffff0010ac4da2534e4a30", -- AW3225
  "00ffffffffffff0010ac49a2534e4a30",
  "00ffffffffffff0010acb3414c323332", -- U2720Q 16x9
  "00ffffffffffff0010acb7414c323332", -- U2720Q 9x16
  "00ffffffffffff0010acb4414c323332",
  "00ffffffffffff0010acb5414c323332",
  "00ffffffffffff00061044a000000000", -- Laptop display
  "00ffffffffffff0010acb3414c333232", -- U2720Q 9x16
  "00ffffffffffff0010acb5414c333232"]

-- SD
mySDConfig = def {
               activeColor = white
             , inactiveColor = black
             , urgentColor = "white"
             , activeTextColor = black
             , inactiveTextColor = white
             , urgentTextColor = "red"
             , activeBorderColor = white
             , inactiveBorderColor = black
             , urgentBorderColor = "pink"
             , decoHeight = 32
             , fontName = "xft:monospace-9:bold,Symbola-9:bold"
}

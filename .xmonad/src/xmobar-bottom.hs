import Xmobar

import Control.Monad (when, void)
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnvDefault)

import Data.List.Split (splitOn)
import qualified Configuration.Dotenv as Dotenv

import qualified QMonad.Shared.Theme as T

-- Config
config :: (String, String, [(String, String)]) -> Config
config (scriptsDir, weatherStation, networkCards) = defaultConfig {
  -- General behavior
    hideOnStart      = False
  , allDesktops      = True
  , overrideRedirect = True
  , persistent       = False

  -- Appearance
  , font            = "xft:Jetbrains Mono:size=11:antialias=true:hinting=true"
  , additionalFonts = [
      "xft:Ubuntu Nerd Font:size=12"
    , "xft:Font Awesome 5 Pro:size=11:style=Solid"
    , "xft:Font Awesome 5 Pro:size=10:style=Regular"
  ]
  , textOffset  = 17
  , textOffsets = [-1, 18, 17]

  , fgColor  = T.fgColor
  , bgColor  = T.bgColor
  , position = Bottom
  , border   = NoBorder

  -- Layout
  , sepChar  = "%"
  , alignSep = "}{"
  , template =
         " "

      ++ "%formattedTime% %" ++ weatherStation ++ "%"
      ++ " "
      ++ "%XmonadInfo%"

      ++ "}"
      ++ "%UnsafeStdinReader%"
      ++ " "
      ++ "%DiscordBell%"
      ++ "{"

      ++ "<action=`networkmanager_dmenu`>"
      ++ concatMap (\(networkCard,_) -> "%" ++ networkCard ++ "%") networkCards
      ++ "</action>"
      ++ "%battery%<action=`pavucontrol`>%vol%</action>"
      ++ " "
      ++ "<fc=#be7572>%memory% %cpu% %coretemp%</fc>"
      ++ " "

  -- Commands
  , commands = [
      Run UnsafeStdinReader -- Workspaces
    , Run $ PipeReader "/tmp/xmonad-info" "XmonadInfo" -- Layout, title
    , Run $ PipeReader "<fn=0></fn>:/tmp/discord.pipe" "DiscordBell"

    , Run $ Memory   ["-t", "<fn=2>\xf538</fn><fn=1> </fn><usedratio>%"] 10
    , Run $ Cpu ["-t", "<fn=2>\xf2db</fn><fn=1> </fn><total>%", "-m", "2" , "-c", " "] 10
    , Run $ CoreTemp ["-t","<fn=2>\xf2c8</fn><fn=1> </fn><core0>°C"   ] 50

    , Run $ Weather weatherStation ["-t", "<tempC>°C", "-x", "<fn=0></fn>"] 18000
    , Run $ Battery ["-t", "<acstatus>", "--", "-o", "<fn=2>\xf242</fn><fn=1> </fn><left>% ", "-O", "<fn=2>\xf376</fn> <left>% ", "-i", ""] 50
    , Run $ Com "sh" [scriptsDir ++ "pulseaudio-display.sh"] "vol" 30
    , Run $ Date ("%a-%d <fc=" ++ T.brightFgColor ++ "><fn=2>\xf017</fn><fn=1> </fn>%I:%M:%S<fn=1> </fn>%p</fc>") "formattedTime" 10

  -- Network cards dynamic commands
  ] ++ map (\(networkCard, icon) -> Run $ Network networkCard [
      "-t", "<fc=#98bcbd><rx>kB<fn=1> </fn><fn=2>\xf063</fn></fc>"
         ++ "<fn=1> </fn><fc=" ++ T.darkFgColor ++ "><fn=2>" ++ icon ++ "</fn></fc><fn=1> </fn>"
         ++ "<fn=2>\xf062</fn><fn=1> </fn><tx>kB"
    , "-m", "2", "-c", " ", "-x", "~" ++ networkCard ++ "~"
  ] 10) networkCards
}

-- Main
main :: IO()
main = do
  -- Environment variables
  let envCfg = Dotenv.defaultConfig
  exists <- doesFileExist $ head (Dotenv.configPath envCfg)
  when exists $
    void $ Dotenv.loadFile envCfg
  weatherStation <- getEnvDefault "WEATHER_STATION" "LFLY"
  networkCards'  <- getEnvDefault "NETWORK_CARDS"   "eth0:\xf108"

  xmonadDir <- getEnvDefault "XMONAD" "../.."
  xmobar $ config (xmonadDir ++ "/scripts/",
                   weatherStation,
                   map (mkTuple . splitOn ":") (splitOn "," networkCards'))

mkTuple :: [String] -> (String, String)
mkTuple [a,b] = (a,b)


import Xmobar

import Control.Monad (when, void)
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnvDefault)

import Data.List.Split (splitOn)
import qualified Configuration.Dotenv as Dotenv

import qualified QMonad.Shared.Theme as T

-- Config
config :: (String, String, [(String, String)], Int) -> Config
config (scriptsDir, weatherStation, networkCards, fontTailor) = defaultConfig {
  -- General behavior
    hideOnStart      = False
  , allDesktops      = True
  , overrideRedirect = True
  , persistent       = False

  -- Appearance
  , font            = "xft:Jetbrains Mono:size=" ++ show (11 + fontTailor) ++ ":antialias=true:hinting=true"
  , additionalFonts = [
      "xft:Ubuntu Nerd Font:size=" ++ show (10 + fontTailor)
    , "xft:Font Awesome 5 Pro:size=" ++ show (10 + fontTailor) ++ ":style=Solid"
  ]
  , textOffset  = 16
  , textOffsets = [-1, 16]

  , fgColor  = "#848089"
  , bgColor  = "#191919"
  , alpha    = 210
  , position = Top
  , border   = NoBorder

  -- Layout
  , sepChar  = "%"
  , alignSep = "}{"
  , template =
         " "

      ++ "<fc=#be7572>%memory% %cpu% %multicoretemp%</fc>"
      ++ " "
      ++ "%XmonadInfo%"
      ++ "}"
      ++ "%UnsafeStdinReader% %DiscordBell%"
      ++ "{"

      ++ "%battery% <action=`pavucontrol`>%vol%</action>"
      ++ " "
      ++ "<action=`networkmanager_dmenu`>"
      ++ concatMap (\(networkCard,_) -> "%" ++ networkCard ++ "%") networkCards
      ++ "</action>"
      ++ " "
      ++ "%" ++ weatherStation ++ "% %formattedTime%"
      ++ " "

  -- Commands
  , commands = [
      Run UnsafeStdinReader -- Workspaces
    , Run $ PipeReader "/tmp/xmonad-info" "XmonadInfo" -- Layout, title
    , Run $ PipeReader "<fn=0></fn>:/tmp/discord.pipe" "DiscordBell"

    , Run $ Memory   ["-t", "<fn=2>\xf538</fn><fn=1> </fn><usedratio>%"] 10
    , Run $ Cpu ["-t", "<fn=2>\xf2db</fn><fn=1> </fn><total>%", "-m", "2" , "-c", " "] 10
    , Run $ MultiCoreTemp ["-t","<fn=2>\xf2c8</fn><fn=1> </fn><max>°C"] 50

    , Run $ Weather weatherStation ["-t", "<tempC>°C", "-x", "<fn=0></fn>"] 18000
    , Run $ Battery ["-t", "<acstatus>", "--", "-o", "<fn=2>\xf242</fn><fn=1> </fn><left>%", "-O", "<fn=2>\xf376</fn> <left>%", "-i", ""] 50
    , Run $ Com "sh" [scriptsDir ++ "get-volume"] "vol" 30
    , Run $ Date ("<fc=" ++ T.brightFgColor ++ "><fn=2>\xf017</fn><fn=1> </fn>%I:%M:%S<fn=1> </fn>%p</fc> %a-%d") "formattedTime" 2

  -- Network cards dynamic commands
  ] ++ map (\(networkCard, icon) -> Run $ Network networkCard [
      "-t", "<fc=#72beb3><rx>kB<fn=1> </fn><fn=2>\xf063</fn></fc>"
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
  fontTailor <- getEnvDefault "FONT_TAILOR" "0"

  xmonadDir <- getEnvDefault "XMONAD" "../.."
  xmobar $ config (xmonadDir ++ "/bin/statusbar/",
                   weatherStation,
                   map (mkTuple . splitOn ":") (splitOn "," networkCards'),
                   read fontTailor)

mkTuple :: [String] -> (String, String)
mkTuple [a,b] = (a,b)


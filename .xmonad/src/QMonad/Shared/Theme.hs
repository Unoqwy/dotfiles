module QMonad.Shared.Theme where

font :: Int -> String
font s = "xft:Jetbrains Mono:size=" ++ show s ++ ":antialias=true:hinting=true"

secondFont :: Int -> String
secondFont s = "xft:Ubuntu Nerd Font:size=10:antialias=true"

---------------------- Colors ----------------------
primaryColor  = "#85dacc"
secondColor   = "#948ae3"

fgColor       = "#848089"
fgOnPrimColor = "#303030"
brightFgColor = "#d5d0c0"
darkFgColor   = "#848089"

bgColor       = "#191919"


module QMonad.Shared.Theme where

font :: Int -> String
font s = "xft:Jetbrains Mono:size=" ++ show s ++ ":antialias=true:hinting=true"

secondFont :: Int -> String
secondFont s = "xft:Ubuntu Nerd Font:size=11:antialias=true"

---------------------- Colors ----------------------
primaryColor  = "#d7afaf"
secondColor   = "#be7572"

fgColor       = "#9a9898"
fgOnPrimColor = "#303030"
brightFgColor = "#d5d0c0"
darkFgColor   = "#60656f"

bgColor       = "#212337"


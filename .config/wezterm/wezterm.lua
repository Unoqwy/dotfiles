-- The purpose of this configuration is to serve as a fallback terminal emulator.
-- I will keep using alacritty [for now] because it suits my needs better.

-- Noticed behaviors:
-- cons:
-- * window title only displays prog without args
-- * unicodes (e.g. clock) don't render as in alacritty, bc of fallback?
-- * cursor color doesn't update depending on underlying char
-- * no vi mode? :(
-- * no way of [truly] disabling tabs and multiplexing? they might as well add a builtin system-monitor
-- pros:
-- * no need to use the 9.5 font size hack, 10 works (tho it looks a bit bolder on some chars?)
-- * ligatures!

local wezterm = require("wezterm")

return {
    font = wezterm.font("JetBrains Mono"),
    font_rules = {{
        italic = true,
        font = wezterm.font("JetBrains Mono", { italic = true }),
    }, {
        intensity = "Bold",
        font = wezterm.font("JetBrains Mono", { weight = "Medium" }),
    }},
    font_size = 10,

    enable_tab_bar = false,

    color_scheme = "sonokai",
    color_schemes = {
        sonokai = require("sonokai_colorscheme"),
    },
}

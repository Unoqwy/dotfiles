-----------------
-- Option types
-----------------
Themes = {
    Sonokai = 1,
}

-------------------
-- Shared options
-------------------
opts = {
    indent_level = 4,
    theme = Themes.Sonokai,
    time_tracking = true,
    wrap_lines = {
        default = '120',
	['80'] = {'toml', 'yaml'},
    },
}


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
    theme = Themes.Sonokai,

    time_tracking = false, -- install wakatime plugin?
    repl = false, -- install codi plugin?

    indent_level = 4,
    wrap_lines = {
        default = '120',
        ['80'] = {'toml', 'yaml'},
    },
}


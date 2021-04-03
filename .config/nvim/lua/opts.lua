-----------------
-- Option types
-----------------
_G.Themes = {
    Sonokai = 1,
}

_G.Languages = {
    Lua = 'lua',
    Rust = 'rust',
}

-------------------
-- Shared options
-------------------
_G.opts = {
    theme = Themes.Sonokai,

    time_tracking = false, -- install wakatime plugin?
    repl = false, -- install codi plugin?
    better_comments = true, -- give some powers to comments
    lsp = true, -- configure lsp clients

    indent_level = 4,
    wrap_lines = {
        default = '120',
        ['80'] = {'toml', 'yaml'},
    },

    -- which supported languages should be installed/managed?
    languages = {
        Languages.Lua,
        Languages.Rust,
    },
    handles = function(lang) return table.includes(opts.languages, lang) end,
}


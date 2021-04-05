-----------------
-- Option types
-----------------
_G.Themes = {
    Sonokai = 1,
}

_G.Languages = {
    Java = 'java',
    Lua = 'lua',
    Rust = 'rust',
    Zig = 'zig',
}

-------------------
-- Shared options
-------------------
_G.opts = {
    theme = Themes.Sonokai,

    time_tracking = false, -- install wakatime plugin?
    repl = false, -- install codi plugin?
    better_comments = true, -- give some powers to comments
    smart_pairs = true, -- auto close pairs
    lsp = true, -- configure lsp clients

    indent_level = 4,
    wrap_lines = {
        default = '120',
        ['80'] = {'toml', 'yaml'},
    },

    -- which supported languages should be installed/managed?
    languages = {
        Languages.Java,
        Languages.Lua,
        Languages.Rust,
        Languages.Zig,
    },
    -- languages that do not need LSP support or whatsoever, just being installed with treesitter
    treesitter_additional_languages = {
        'bash',
        'json',
        'jsonc',
        'regex',
        'toml',
    },
    handles = function(lang) return table.includes(opts.languages, lang) end,
}


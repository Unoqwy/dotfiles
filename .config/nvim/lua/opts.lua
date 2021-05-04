-----------------
-- Option types
-----------------
_G.Themes = {
    Sonokai = 1,
}

_G.Languages = {
    Java = 'java',
    Lua = 'lua',
    PHP = 'php',
    Rust = 'rust',
    Zig = 'zig',
}

_G.Tools = {
    Symfony = 'symfony',
}

-------------------
-- Shared options
-------------------
_G.opts = {
    time_tracking = false, -- install wakatime plugin?

    -- editing
    text = false, -- use vim for text editing

    better_comments = true, -- give some powers to comments
    smart_pairs = true, -- auto close pairs

    -- vcs
    git = true, -- plugins for git integration

    -- code
    lsp = false, -- configure lsp clients
    snippets = false, -- handle and preconfigure snippets
    deps_tools = false, -- install tools to manage dependencies for some languages / techs

    -- debug
    repl = false, -- install codi plugin?
    cfg_tools = false, -- install/config tools to help configure vim

    -- style
    theme = Themes.Sonokai,
    indent_level = 4,
    wrap_lines = {
        default = '120',
        ['100,120'] = {'rust'},
        ['80'] = {'toml', 'yaml'},
    },

    -- which supported languages should be installed/managed?
    languages = {
        Languages.Lua,
        Languages.Rust,
    },
    tools = {},

    -- languages that do not need LSP support or whatsoever, just being installed with treesitter
    treesitter_additional_languages = {
        'bash',
        'json',
        'jsonc',
        'regex',
        'toml',
    },
    handles = function(lang) return vim.tbl_contains(opts.languages, lang) end,
}

local M = {}

function M.preset_desktop()
    opts.text = true
    opts.lsp = true
    opts.snippets = true
    opts.deps_tools = true

    opts.languages = {
        Languages.Lua,
        Languages.Rust,
        Languages.Zig,
    }
end

function M.preset_ext_web()
    q.vec_ext(opts.languages, {Languages.PHP})
    q.vec_ext(opts.tools, {Tools.Symfony})
end

return M


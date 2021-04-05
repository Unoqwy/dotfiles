-- 'q' -> custom api for config
_G.q = {}

--> global directories
_G.q.ext_dir = vim.fn.stdpath('config') .. '/ext'

require('utils') -- global utils

require('opts') -- load opts
require('overrides') -- local overrides

require('settings') -- apply settings
require('keybindings') -- bindings helper methods

--> preload modules before loading plugins
local theme = require('theme')
local editor = require('editor')

--> manage plugins
local plugins = require('plugins')
q.safe_call(function()
    plugins.pre_install() -- make sure Packer is installed
    plugins.install {theme, editor}
end)

--> init modules
q.safe_call(theme.init)
q.safe_call(editor.init)

--> key mappings
q.safe_call(require('keybindings').register_defaults)


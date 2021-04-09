-- 'q' -> custom api for config
if _G.q then
    -- reloading config
    _G.q = { already_loaded = true }
else
    _G.q = {}
end

--> global directories
_G.q.ext_dir = vim.fn.stdpath('config') .. '/ext'

require('utils') -- global utils

require('opts') -- load opts
pcall(require, 'overrides') -- local overrides if file exists

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
if not q.already_loaded then
    q.safe_call(theme.init)
end
q.safe_call(editor.init)

--> key mappings
q.safe_call(require('keybindings').register_defaults)


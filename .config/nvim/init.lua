require('utils') -- global utils

require('opts') -- load opts
require('overrides') -- local overrides

require('settings') -- apply settings
require('keybindings') -- bindings helper methods

-- preload modules before loading plugins
local theme = require('theme')
local editor = require('editor')

-- manage plugins
local plugins = require('plugins')
plugins.pre_install() -- make sure Packer is installed
plugins.install {theme, editor}

-- init modules
pcall(theme.init)
pcall(editor.init)

-- key mappings
require('keybindings').register_defaults()


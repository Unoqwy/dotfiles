require('opts') -- load opts
require('settings') -- apply settings

-- preload modules before loading plugins
local theme = require('theme')
local editor = require('editor')

-- manage plugins
local plugins = require('plugins')
plugins.pre_install() -- make sure Packer is installed
plugins.install {theme, editor}

-- init modules
theme.init()
editor.init()

require('keybindings')


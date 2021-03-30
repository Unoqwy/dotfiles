-----------------
-- Set settings
-----------------
vim.o.hidden = true

vim.wo.number         = true
vim.wo.relativenumber = true
vim.wo.signcolumn     = "yes:1"

vim.bo.expandtab    = true
vim.bo.tabstop      = opts.indent_level
vim.bo.shiftwidth   = opts.indent_level

vim.bo.autoindent  = true
vim.bo.smartindent = true


-----------------
-- Set settings
-----------------

--> Global
vim.o.smarttab = true
vim.o.showmode = false
vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.hidden = true
vim.o.splitbelow = true
vim.o.splitright = true

--> Window
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.signcolumn = "yes:1"
vim.wo.list = true

vim.wo.wrap = true
vim.wo.linebreak = true

--> Buffer
vim.bo.expandtab = true
vim.bo.tabstop = opts.indent_level
vim.bo.shiftwidth = opts.indent_level

vim.bo.autoindent = true
vim.bo.smartindent = true


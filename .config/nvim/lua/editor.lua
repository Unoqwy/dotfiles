local function init()

--> Undo history
local undo_dir = vim.fn.stdpath('data') .. '/undo-history/'
if not vim.fn.isdirectory(undo_dir) then
    vim.fn.mkdir(undo_dir, 'p')
end
vim.o.undodir = undo_dir
vim.bo.undofile = true

--> Treesitter
require('nvim-treesitter.configs').setup({
    ensure_installed = {'rust', 'lua'},
    highlight = {
        enabled = true
    },
})

end
return {
    init = init,
    install_deps = function(use)
        use('nvim-treesitter/nvim-treesitter')
    end,
}


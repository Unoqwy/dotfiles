local function init()

--> Undo history
local undo_dir = vim.fn.stdpath('data') .. '/undo-history/'
if not vim.fn.isdirectory(undo_dir) then
    vim.fn.mkdir(undo_dir, 'p')
end
q.o.undodir = undo_dir
q.bo.undofile = true

--> Treesitter
require('nvim-treesitter.configs').setup({
    ensure_installed = {'rust', 'lua'},
    highlight = {
        enabled = true
    },
})

--> Wrap guide
q.wo.colorcolumn = opts.wrap_lines['default']
for k,v in pairs(opts.wrap_lines) do
    if k ~= 'default' then
        local file_types = table.concat(v, ',')
        vim.cmd('au FileType ' .. file_types .. ' set cc=' .. k)
    end
end

end
return {
    init = init,
    install_deps = function(use)
        use('nvim-treesitter/nvim-treesitter')
    end,
}


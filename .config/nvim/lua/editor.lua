local elangs = require('editor.langs')

local M = {}

function M.init()
    --> Undo history
    local undo_dir = vim.fn.stdpath('data') .. '/undo-history/'
    if not vim.fn.isdirectory(undo_dir) then
        vim.fn.mkdir(undo_dir, 'p')
    end
    q.o.undodir = undo_dir
    q.bo.undofile = true

    --> Treesitter
    require('nvim-treesitter.configs').setup({
        ensure_installed = elangs.treesitter_languages(),
        highlight = {
            enable = true
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

    --> Comments
    require('nvim_comment').setup()

    --> Telescope
    require('telescope').setup{
        defaults = {
            mappings = require('keybindings').telescope_mappings(),
        }
    }

    --> Completion
    q.o.completeopt = 'menuone,noinsert'

    --> Languages
    elangs.init()
end

function M.install_deps(use)
    use('nvim-treesitter/nvim-treesitter')

    use('terrortylor/nvim-comment')
    use('axelf4/vim-strip-trailing-whitespace')

    use({
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    })

    if opts.repl then
        use 'metakirby5/codi.vim'
    end

    use('nvim-lua/completion-nvim')
    if opts.lsp then
        use('neovim/nvim-lspconfig')
        use('kabouzeid/nvim-lspinstall')
    end
    elangs.install_deps(use)
end

return M


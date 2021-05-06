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
            enable = true,
        },
        indent = {
            enable = false,
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

    if opts.text then
        vim.g.markdown_fenced_languages = {'vim', 'lua'}
    end

    --> Comments
    require('nvim_comment').setup()

    --> Telescope
    require('telescope').setup{
        defaults = {
            mappings = require('keybindings').telescope_mappings(),

            entry_prefix = ' ',
            selection_caret = ' ',
            borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
            layout_strategy = 'horizontal',
        }
    }
    require('telescope').load_extension('fzy_native')

    --> Completion
    q.o.completeopt = 'menuone,noselect' -- required for nvim-compe, noselect is reverted in compe's config
    q.o.shortmess = vim.o.shortmess .. 'c'

    require('compe').setup({
        enabled = true,
        autocomplete = true,
        min_length = 1,
        preselect = 'always',
        documentation = true,

        source = {
            path = { priority = 10 },
            buffer = { priority = 10 },
            calc = { priority = 5 },
            nvim_lsp = { priority = 20 },
            nvim_lua = { priority = 20 },
            vsnip = { priority = 20 },
        };
    })
    vim.g.lexima_no_default_rules = true;
    vim.fn['lexima#set_default_rules']()

    --> Languages
    elangs.init()

    --> LSP extensions
    if opts.lsp then
        vim.cmd("au InsertLeave,BufEnter,BufWinEnter,TabEnter,BufWritePost *.rs :lua require('editor').inlay_hints()")
    end

    --> LSP Saga
    require('lspsaga').init_lsp_saga(vim.tbl_extend('keep', require('keybindings').lspsaga_mappings(), {
        code_action_prompt = {
            enable = true,
            sign = false,
            virtual_text = false,
        },
    }))

    --> Git
    require('gitsigns').setup({
        signs = {
            add = {text = ' │'},
            change = {text = ' │'},
            delete = {text = ' _'},
            topdelete = {text = ' ‾'},
            changedelete = {text = ' ~'},
        },
        sign_priority = 9, -- lsp has priority
    })
end

function M.inlay_hints()
    require('lsp_extensions').inlay_hints({
        highlight = "InlayHints",
        prefix = "➾ ",
        enabled = { "TypeHint", "ChainingHint", "ParameterHint" },
    })
end

function M.install_deps(use)
    use('nvim-lua/plenary.nvim')
    use('nvim-lua/popup.nvim')

    use('nvim-treesitter/nvim-treesitter')

    use('tpope/vim-surround')
    use('terrortylor/nvim-comment')
    use('axelf4/vim-strip-trailing-whitespace')

    use({
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-telescope/telescope-fzy-native.nvim'}}
    })

    if opts.text then
        use('tpope/vim-markdown')
        use('preservim/vim-pencil')
    end

    if opts.git then
        use('tpope/vim-fugitive')
        use('lewis6991/gitsigns.nvim')
    end

    use('hrsh7th/nvim-compe')
    if opts.smart_pairs then
        use('cohama/lexima.vim')
    end
    if opts.snippets then
        use('hrsh7th/vim-vsnip')
    end
    if opts.lsp then
        use('neovim/nvim-lspconfig')
        use('kabouzeid/nvim-lspinstall')
        use('nvim-lua/lsp_extensions.nvim')
        use('glepnir/lspsaga.nvim')
    end
    elangs.install_deps(use)

    if opts.repl then
        use('metakirby5/codi.vim')
    end
    if opts.cfg_tools then
        use('rafcamlet/nvim-luapad')
    end
end

return M


local nvim_protocol = require('vim.lsp.protocol')
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
            enable = true,
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
    q.o.completeopt = 'menuone,noselect' -- required for nvim-compe, noselect is reverted in compe's config
    q.o.shortmess = vim.o.shortmess .. 'c'

    -- TODO: look into smart completion because bog-standard completion is insanity
    -- maybe aca/completion-tabnine?
    vim.api.nvim_set_var('completion_matching_strategy_list', {'exact', 'substring'})
    vim.api.nvim_set_var('completion_matching_ignore_case', 0)
    vim.api.nvim_set_var('completion_matching_smart_case', 1)
    vim.api.nvim_set_var('completion_sorting', 'length')

    vim.api.nvim_set_var('completion_trigger_keyword_length', 1)

    require('compe').setup({
        enabled = true,
        autocomplete = true,
        min_length = 1,
        preselect = 'always',
        documentation = true,

        source = {
            path = true,
            buffer = true,
            calc = true,
            nvim_lsp = true,
            nvim_lua = true,
            vsnip = true,
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
end

function M.inlay_hints()
    require('lsp_extensions').inlay_hints({
        highlight = "InlayHints",
        prefix = "➾ ",
        enabled = { "TypeHint", "ChainingHint", "ParameterHint" },
    })
end

function M.install_deps(use)
    use('nvim-treesitter/nvim-treesitter')

    use('tpope/vim-surround')
    use('terrortylor/nvim-comment')
    use('axelf4/vim-strip-trailing-whitespace')

    use({
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    })

    if opts.repl then
        use 'metakirby5/codi.vim'
    end

    use('hrsh7th/nvim-compe')
    if opts.smart_pairs then
        use('cohama/lexima.vim')
    end

    if opts.lsp then
        use('neovim/nvim-lspconfig')
        use('kabouzeid/nvim-lspinstall')
        use('nvim-lua/lsp_extensions.nvim')
        use('glepnir/lspsaga.nvim')
    end
    elangs.install_deps(use)

    if opts.cfg_tools then
        use('rafcamlet/nvim-luapad')
    end
end

return M


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
    q.o.completeopt = 'menuone,noinsert'
    q.o.shortmess = vim.o.shortmess .. 'c'

    -- TODO: look into smart completion because bog-standard completion is insanity
    -- maybe aca/completion-tabnine?
    vim.api.nvim_set_var('completion_matching_strategy_list', {'exact', 'substring'})
    vim.api.nvim_set_var('completion_matching_ignore_case', 0)
    vim.api.nvim_set_var('completion_matching_smart_case', 1)
    vim.api.nvim_set_var('completion_sorting', 'length')

    vim.api.nvim_set_var('completion_trigger_keyword_length', 1)

    _G.q.completion = {
        map = function(tbl, kind, display_name)
            if type(kind) == 'table' and not display_name then
                for k,n in pairs(kind) do
                    if k ~= '_' then
                        q.completion.map(tbl, k, n)
                    end
                end

                -- yes, this is very hacky
                local surround = kind['_']
                if surround then
                    local lhs, rhs = surround[1] or '', surround[2] or surround[1]
                    local transform = type(surround[3]) == 'function' and surround[3] or function(n) return n end

                    local tbl_ref = nvim_protocol[tbl]
                    local labels = require('completion.option').get_option('customize_lsp_label')
                    for n,idx in pairs(tbl_ref) do
                        if type(n) == 'string' and type(idx) == 'number' then
                            labels[n] = lhs .. transform(labels[n] or tbl_ref[idx]) .. rhs
                        end
                    end
                end
            elseif type(kind) == 'string' and display_name then
                local labels = require('completion.option').get_option('customize_lsp_label')
                labels[kind] = display_name
            end
        end,
        on_attach = function()
            require('completion').on_attach({ customize_lsp_label = {} })
            require('theme').attach_completion(q.completion.map)
        end,
    }

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

    use('nvim-lua/completion-nvim')
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


local M = {}

local function reload_lsp()
    local lspinstall = require('lspinstall')
    local lspconfig = require('lspconfig')

    lspinstall.setup()
    for _,server in pairs(lspinstall.installed_servers()) do
        lspconfig[server].setup({})
    end
end

function M.init()
    if opts.lsp then
        reload_lsp()
        require('lspinstall').post_install_hook = function()
            reload_lsp()
            vim.cmd('bufdo e')
        end
    end
end

function M.treesitter_languages()
    local parsers = require('nvim-treesitter.parsers')
    local parserlist = parsers.available_parsers()

    local ensure_installed = {}
    for _,lang in ipairs(opts.languages) do
        if table.includes(parserlist, lang) then
            table.insert(ensure_installed, lang)
        end
    end
    if opts.better_comments then
        table.insert(ensure_installed, 'comment')
    end
    return ensure_installed
end

function M.install_deps(use)
    if opts.handles(Languages.Lua) then
        use('tbastos/vim-lua')
    end
    if opts.handles(Languages.Rust) then
        use('rust-lang/rust.vim')
    end
end

return M


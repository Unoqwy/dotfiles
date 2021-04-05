local M = {}

local function common_attach()
    require('completion').on_attach()
end

local function reload_lsp()
    local lspinstall = require('lspinstall')
    local lspconfig = require('lspconfig')

    lspinstall.setup()
    for _,server in pairs(lspinstall.installed_servers()) do
        lspconfig[server].setup({on_attach=common_attach})
    end
end

function M.get(lang)
    local status,elang = pcall(require, 'editor.langs.' .. lang)
    if not status then
        return nil
    end
    return elang
end

function M.init()
    if opts.lsp then
        q.lsp = {
            on_attach = common_attach,
        }

        reload_lsp()
        require('lspinstall').post_install_hook = function()
            reload_lsp()
            vim.cmd('bufdo e')
        end
    end

    for _,lang in ipairs(opts.languages) do
        local elang = M.get(lang)
        if elang and elang.setup_lsp then
            elang.setup_lsp()
        end
    end

    if opts.handles(Languages.Zig) then
        vim.g.zig_fmt_autosave = 0
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
    local deps = {
        [Languages.Java] = {'mfussenegger/nvim-jdtls'},
        [Languages.Lua] = {'tbastos/vim-lua'},
        [Languages.Rust] = {'rust-lang/rust.vim'},
        [Languages.Zig] = {'ziglang/zig.vim'},
    }
    for _,lang in ipairs(opts.languages) do
        if deps[lang] then
            for _,dep in ipairs(deps[lang]) do
                use(dep)
            end
        end
    end
end

return M


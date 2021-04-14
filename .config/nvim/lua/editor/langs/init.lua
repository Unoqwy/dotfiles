local M = {}

local function setup_lsp(reload)
    local lspinstall = require('lspinstall')
    local lspconfig = require('lspconfig')

    lspinstall.setup()
    for _,lang in ipairs(opts.languages) do
        local elang = M.get(lang)
        if elang then
            if elang.setup_lsp and (elang.supports_reload or not reload) then
                elang.setup_lsp()
            end
        elseif lspconfig[lang] then
            lspconfig[lang].setup({ on_attach = q.lsp.on_attach })
        end
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
            on_attach = function()
            end,
        }

        setup_lsp(false)
        require('lspinstall').post_install_hook = function()
            setup_lsp(true)
            vim.cmd('bufdo e')
        end
    end

    if opts.handles(Languages.Zig) then
        vim.g.zig_fmt_autosave = 1
    end
    if opts.handles(Languages.Rust) then
        vim.g.rustfmt_autosave = 1
    end
end

function M.treesitter_languages()
    local parsers = require('nvim-treesitter.parsers')
    local parserlist = parsers.available_parsers()

    local ensure_installed = {}
    for _,lang in ipairs(opts.languages) do
        if vim.tbl_contains(parserlist, lang) then
            table.insert(ensure_installed, lang)
        end
    end
    for _,lang in ipairs(opts.treesitter_additional_languages) do
        if not vim.tbl_contains(ensure_installed, lang) then
            table.insert(ensure_installed, lang)
        end
    end
    if opts.better_comments and not vim.tbl_contains(ensure_installed, 'comment') then
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

    if opts.handles(Languages.Rust) and opts.deps_tools then
        use('mhinz/vim-crates')
        vim.cmd('au BufRead Cargo.toml call crates#toggle()')
    end
end

return M


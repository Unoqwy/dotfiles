local M = {}
M.supports_reload = true

local function list_libs()
    local libs = {}
    if opts.cfg_tools then
        -- FIXME: this is ugly and while it does the job, some lsp elts are considered str (that's annoying)
        vim.tbl_extend("keep", libs, {
            [vim.fn.expand('$VIMRUNTIME/lua')] = true,
            [vim.fn.expand('$VIMRUNTIME/lua/vim')] = true,
            [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
            [vim.fn.stdpath('config') .. '/lua'] = true,
        })
    end
    return libs
end

function M.setup_lsp()
    require('lspconfig').lua.setup({
        on_attach = q.lsp.on_attach,
        settings = {
            Lua = {
                runtime = {
                    version = 'LuaJIT',
                    path = vim.split(package.path, ';'),
                },
                workspace = {
                    library = list_libs(),
                    maxPreload = 2000,
                    preloadFileSize = 1000,
                },
                diagnostics = {
                    globals = {"vim", "q"},
                },
            }
        }
    })
end

return M


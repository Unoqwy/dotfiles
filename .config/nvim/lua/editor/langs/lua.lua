local M = {}
M.supports_reload = true

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
                    library = {
                        [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                        [vim.fn.expand('$VIMRUNTIME/lua/vim')] = true,
                        [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                        [vim.fn.stdpath('config') .. '/lua'] = true,
                    },
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


local M = {}

function M.setup_lsp()
    require('lspconfig').rust.setup({
        on_attach = q.lsp.on_attach,
        settings = {
            ["rust-analyzer"] = {
                assist = {
                    importMergeBehavior = "last",
                    importPrefix = "by_self",
                },
                procMacro = {
                    enable = true,
                },
            }
        },
    })
end

return M


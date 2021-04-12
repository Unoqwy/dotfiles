local M = {}

function M.setup_lsp()
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities.textDocument.completion.completionItem.resolveSupport = { properties = { 'additionalTextEdits' } }
    require('lspconfig').rust.setup({
        capabilities = capabilities,
        on_attach = q.lsp.on_attach,
        settings = {
            ["rust-analyzer"] = {
                assist = {
                    importMergeBehavior = "last",
                    importPrefix = "by_crate",
                },
                diagnostics = {
                    enable = true,
                    disabled = {"unresolved-proc-macro"},
                },

                cargo = {
                    loadOutDirsFromCheck = true, -- eq. runBuildScripts
                },
                procMacro = {
                    enable = true,
                },
            }
        },
    })
end

return M


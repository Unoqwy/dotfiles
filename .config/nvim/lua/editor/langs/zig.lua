local M = {}

function M.setup_lsp()
    require('lspconfig').zls.setup({on_attach=q.lsp.on_attach})
end

return M


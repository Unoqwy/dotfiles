local M = {}

function M.setup_lsp()
    vim.cmd("autocmd FileType java lua require('editor.langs.java').enter_file()")
end

function M.enter_file()
    if opts.lsp then
        local setup = require('jdtls.setup')
        require('jdtls').start_or_attach({
            cmd = {q.ext_dir .. '/jdtls'},
            on_attach = q.lsp.on_attach,
            root_dir = setup.find_root({'.git', 'pom.xml', 'build.gradle'}),
        })
    end
end

return M


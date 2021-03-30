local function init()

-- Treesitter
require('nvim-treesitter.configs').setup({
    ensure_installed = {'rust', 'lua'},
    highlight = { enabled = true },
})

end
return {
    init = init,
    install_deps = function(use)
        use('nvim-treesitter/nvim-treesitter')
    end,
}


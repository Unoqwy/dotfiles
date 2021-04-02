local function init()

---------------------
-- General settings
---------------------
q.o.termguicolors = true
q.wo.t_Co = '256'

q.o.hlsearch = true
q.wo.cursorline = true

-- fix for tmux (TODO: is it even needed?)
vim.cmd('let &t_8f = "\\<Esc>[38;2;%lu;%lu;%lum"')
vim.cmd('let &t_8b = "\\<Esc>[48;2;%lu;%lu;%lum"')

-------------------
-- Theme settings
-------------------
vim.cmd('syntax on')

--> Set theme
if opts.theme == Themes.Sonokai then
    vim.g.sonokai_disable_italic_comment = 1
    vim.g.sonokai_style = 'spectrum'
    vim.cmd('colorscheme sonokai')
end

--> Special highlights
vim.cmd('hi CocRustTypeHint guifg=#607080')
vim.cmd('hi CocRustChainingHint guifg=#959595')

--> Search
vim.cmd('hi clear Search')
vim.cmd('hi Search gui=underline,bold')

--> Special chars
local invalid_color = '#FF8A65'
vim.cmd('set listchars=trail:·,tab:▷\\ ,nbsp:␣')
vim.cmd('hi SpecialKey guifg=' .. invalid_color)
vim.fn.matchadd('SpecialKey', '\\t')
vim.fn.matchadd('SpecialKey', '\\s\\+$')

----------------
-- Status line
----------------
local galaxyline = require('galaxyline')

-----------------
-- Other tweaks
-----------------
local colorizer = require('colorizer')
colorizer.setup()

vim.g.indent_blankline_char = '▏'
vim.g.indent_blankline_use_treesitter = true

vim.g.indent_blankline_show_first_indent_level = false
vim.g.indent_blankline_show_trailing_blankline_indent = false

end
return {
    init = init,
    install_deps = function(use)
        use('glepnir/galaxyline.nvim')
        use('norcalli/nvim-colorizer.lua')

        -- disabled for now, breaks listchars highlight
        --use {'lukas-reineke/indent-blankline.nvim',branch='lua'}
        use('dominikduda/vim_current_word')

        if opts.theme == Themes.Sonokai then
            use('~/contrib/sonokai')
        end
    end
}


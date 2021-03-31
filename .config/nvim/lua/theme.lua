local function init()

---------------------
-- General settings
---------------------
vim.o.termguicolors = true
vim.wo.t_Co = '256'

vim.o.hlsearch = true
vim.wo.cursorline = true

-------------------
-- Theme settings
-------------------
vim.cmd('syntax on')

--> Set theme
if opts.theme == Themes.Sonokai then
    vim.g.sonokai_disable_italic_comment = 1
    vim.g.sonokai_style = 'shusia'
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

end
return {
    init = init,
    install_deps = function(use)
        use('glepnir/galaxyline.nvim')
        use('norcalli/nvim-colorizer.lua')

        if opts.theme == Themes.Sonokai then
            use('sainnhe/sonokai')
        end
    end
}


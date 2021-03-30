local function init()

-- compat
vim.wo.t_Co = '256'

local function set(key, val) vim.api.nvim_set_var(key, val) end

-----------------------
-- Per-theme settings
-----------------------
local colorscheme
if opts.theme == Themes.Sonokai then
    set('sonokai_lightline_disabled_bold', 1)
    set('sonokai_disable_italic_comment', 1)

    set('sonokai_style', 'andromeda')
    colorscheme = 'sonokai'
end

vim.cmd('syntax on')
vim.cmd('set background=dark')
if colorscheme ~= nil then
    vim.cmd('colorscheme ' .. colorscheme)
end

-------------------------------
-- Theme-independent settings
-------------------------------
vim.cmd('set cursorline')

end
return {
    init = init,
    install_deps = function(use)
        use('glepnir/galaxyline.nvim')

        if opts.theme == Themes.Sonokai then
            use('sainnhe/sonokai')
        end
    end
}


local M = {}

function M.init()
    ---------------------
    -- General settings
    ---------------------
    q.o.termguicolors = true
    q.wo.t_Co = '256'

    q.o.hlsearch = true
    q.wo.cursorline = true

    -- fix for tmux (FIXME: is it even needed?)
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
        if vim.g.sonokai_style == 'spectrum' then
            -- if the function is not called prior to be overrided, it won't be replaced
            vim.fn["sonokai#get_palette"]('default')
            -- load custom palette for sonokai
            vim.cmd('source ' .. vim.fn.stdpath('config') .. '/themes/sonokai.vim')
        end
        vim.cmd('colorscheme sonokai')

        local palette = require('theme.palette')
        vim.cmd('hi Pmenu guibg=' .. palette.bg)
        vim.cmd('hi PmenuThumb guibg=' .. palette.stand_bg)
        vim.cmd('hi PmenuSbar guibg=' .. palette.bg_contrast)
        vim.cmd('hi NormalFloat guibg=' .. palette.bg)
        vim.cmd('hi PmenuSel guifg=' .. palette.blue .. ' guibg=' .. palette.stand_bg)

        vim.cmd('hi Crates guibg=' .. palette.bg_contrast .. ' guifg=' .. palette.purple)

        vim.cmd('hi InlayHints guifg=' .. palette.grey)
    end

    -- load palette
    local palette = require('theme.palette')

    --> Search
    vim.cmd('hi clear Search')
    vim.cmd('hi Search gui=underline,bold')

    --> Special chars
    vim.cmd('set listchars=trail:·,tab:▷\\ ,nbsp:␣')
    vim.cmd('hi SpecialKey guifg=' .. palette.orange)
    vim.fn.matchadd('SpecialKey', '\\t')
    vim.fn.matchadd('SpecialKey', '\\s\\+$')

    -----------------
    -- Other tweaks
    -----------------
    require('colorizer').setup({'*'}, {names = false})
    require('theme.galaxyline')

    require('indent_guides').setup({
        indent_start_level = 2,
        even_colors = { fg = 'TermCursor', bg = palette.bg_contrast },
        odd_colors = { fg = 'TermCursor', bg = palette.bg_contrast },
    })
end

function M.install_deps(use)
    use('kyazdani42/nvim-web-devicons')
    use('glepnir/galaxyline.nvim')
    use('norcalli/nvim-colorizer.lua')

    use('glepnir/indent-guides.nvim')
    use('RRethy/vim-illuminate')
    use('tpope/vim-sleuth')

    if opts.theme == Themes.Sonokai then
        use('sainnhe/sonokai')
    end
end

return M


----------------------
-- Utility functions
----------------------
function nmap(key, action, silent, opts)
    if opts == nil then
        opts = {}
    end
    if opts.noremap == nil then
        opts.noremap = true
    end
    opts.silent = silent
    vim.api.nvim_set_keymap('n', key, action, opts)
end

-------------
-- Mappings
-------------
local function register_defaults()
    nmap('<space>', '<Nop>')
    vim.g.mapleader = ' '

    --> [f]ile
    -- [s]ave; r[eload];
    nmap('<leader>fs', ':w<CR>')
    nmap('<leader>fr', ':e<CR>')

    nmap('<C-P>', [[:lua require('telescope.builtin').find_files()<CR>]])
    -- [g]rep;
    nmap('<leader>fg', [[:lua require('telescope.builtin').live_grep()<CR>]])
    -- [l]ocal; [t]ags
    nmap('<leader>fl', [[:lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]])
    nmap('<leader>ft', [[:lua require('telescope.builtin').current_buffer_tags()<CR>]])

    nmap('<leader>,', '<C-^>')

    --> [t]ransform
    -- [w]hitespaces; [r]etab;
    nmap('<leader>tw', ':StripTrailingWhitespace<CR>', true)
    nmap('<leader>tr', ':retab<CR>', true)

    --> [p]aste
    -- [a]bove; [t]oggle;
    nmap('<leader>pa', 'O<ESC>p')
    nmap('<leader>pt', ':set paste!<CR>', true)

    --> [c]lear
    -- [h]ighlight; [l]ine;
    nmap('<leader>ch', ':nohl<CR>', true)
    nmap('<leader>cl', '^d$', true)

    --> Splits
    for _,v in ipairs({'J', 'K', 'L', 'H'}) do
        nmap('<C-'..v..'>', '<C-W><C-'..v..'>', true)
    end

    --> Quality of Life
    nmap('<leader>d', '"_d') -- delete without yanking

    -- go up/down and re-indent trimmed line
    nmap('<leader>k', 'kddO')
    nmap('<leader>j', 'jddO')
end
return { register_defaults = register_defaults }


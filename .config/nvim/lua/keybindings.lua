----------------------
-- Utility functions
----------------------
local function nmap(key, action, silent, noremap)
    if noremap == nil then
        noremap = true
    end
    vim.api.nvim_set_keymap('n', key, action, {silent = silent, noremap = noremap})
end

-------------
-- Mappings
-------------
nmap('<space>', '<Nop>')
vim.g.mapleader = ' '

-- f(ile) shortcuts
-- s(ave)
nmap('<leader>fs', ':w<CR>') 

-- splits
for _,v in ipairs({'J', 'K', 'L', 'H'}) do
    nmap('<C-'..v..'>', '<C-W><C-'..v..'>', true)
end

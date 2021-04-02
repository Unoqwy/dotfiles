local M = {}

----------------------
-- Function bindings
----------------------
local func_bindings = {}
local func_current_id = 0

function M.call(func_id)
    if func_bindings[func_id] then
        func_bindings[func_id]()
    end
end

----------------------
-- Utility functions
----------------------
function _G.q.map(mode, key, action, opts)
    opts = opts ~= nil and opts or {}

    if opts.silent == nil then opts.silent = true end
    if opts.noremap == nil then opts.noremap = true end

    if type(action) == 'function' then
        func_current_id = func_current_id + 1
        local func_id = mode .. '(' .. func_current_id .. ')'
        func_bindings[func_id] = action
        vim.api.nvim_set_keymap(mode, key, ":lua require('keybindings').call('" .. func_id .. "')<CR>", opts)
    else
        vim.api.nvim_set_keymap(mode, key, action, opts)
    end
end

local nmap = function(key, action, opts) q.map('n', key, action, opts) end
local imap = function(key, action, opts) q.map('i', key, action, opts) end

-------------
-- Mappings
-------------
function M.register_defaults()
    nmap('<space>', '<Nop>')
    vim.g.mapleader = ' '

    --> [f]ile
    -- [s]ave; r[eload];
    nmap('<leader>fs', ':w<CR>')
    nmap('<leader>fr', ':e<CR>')

    nmap('<C-P>', function() require('telescope.builtin').find_files() end)
    nmap('<C-M>', function() require('telescope.builtin').git_files() end)
    nmap('<leader>.', function() require('telescope.builtin').file_browser() end)
    -- [g]rep;
    nmap('<leader>fg', function() require('telescope.builtin').live_grep() end)
    -- [l]ocal; [t]ags
    nmap('<leader>fl', function() require('telescope.builtin').current_buffer_fuzzy_find() end)
    nmap('<leader>ft', function() require('telescope.builtin').current_buffer_tags() end)

    nmap('<leader>,', '<C-^>') -- previous file

    --> [t]ransform
    -- [w]hitespaces; [r]etab;
    nmap('<leader>tw', ':StripTrailingWhitespace<CR>')
    nmap('<leader>tr', ':retab<CR>')

    --> [p]aste
    -- [t]oggle;
    nmap('<leader>pt', ':set paste!<CR>')

    --> [c]lear
    -- [h]ighlight; [l]ine;
    nmap('<leader>ch', ':nohl<CR>')
    nmap('<leader>cl', '^d$')

    --> Splits
    for _,v in ipairs({'J', 'K', 'L', 'H'}) do
        nmap('<C-'..v..'>', '<C-W><C-'..v..'>')
    end

    --> Quality of Life
    nmap('<leader>d', '"_d') -- delete without yanking

    -- go up/down and re-indent trimmed line
    nmap('<leader>k', 'kddO')
    nmap('<leader>j', 'jddO')
end

function M.telescope_mappings()
    local actions = require('telescope.actions')

    local utils = require('telescope.utils')
    local action_state = require('telescope.actions.state')

    local delete_file = function(prompt_bufnr)
        local cwd = action_state.get_current_picker(prompt_bufnr).cwd
        local selection = action_state.get_selected_entry()

        local confirmation = vim.fn.input('Do you really wanna delete file' .. selection.value .. '? [y/N] ')
        if string.lower(confirmation) ~= 'y' then return end

        local _, ret, stderr = utils.get_os_command_output({ 'rm', '-rf', selection.value }, cwd)
        if ret == 0 then
            print(" Deleted file: " .. selection.value)
        else
            print(string.format('Could not delete file. Cause: "%s"', table.concat(stderr, '  ')))
        end
    end

    return {
        n = {
            ["<C-C>"] = actions.close,
            ["<C-Z>"] = delete_file,
        },
        i = {
            ["<C-K>"] = actions.move_selection_previous,
            ["<C-J>"] = actions.move_selection_next,
        },
    }
end

return M


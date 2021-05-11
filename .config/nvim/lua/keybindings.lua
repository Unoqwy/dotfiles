local spyglass = require('editor.spyglass')

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

        local prefix = "<CMD>"
        if mode == 't' then
            prefix = "<C-\\><C-n>:"
        end
        vim.api.nvim_set_keymap(mode, key, prefix .. "lua require('keybindings').call('" .. func_id .. "')<CR>", opts)
    else
        vim.api.nvim_set_keymap(mode, key, action, opts)
    end
end

local nmap = function(key, action, opts) q.map('n', key, action, opts) end
local imap = function(key, action, opts) q.map('i', key, action, opts) end
local vmap = function(key, action, opts) q.map('v', key, action, opts) end

-------------
-- Mappings
-------------
local function mod_bindings(module_name, callback, silent)
    local status, mod = pcall(require, module_name)
    if status then
        callback(mod)
    elseif not silent then
        print("[ERR/Keybindings] Could not set bindings depending on module '" .. module_name .. "'")
    end
end

function M.register_defaults()
    --> Unset annoying keybindings
    nmap('Q', '<Nop>') -- dead is Ex mode, Q stands for Quick Fix, change my mind

    nmap('<space>', '<Nop>')
    vim.g.mapleader = ' '

    --> [f]ile
    -- [s]ave; r[eload];
    nmap('<leader>fs', ':w<CR>')
    nmap('<leader>fr', ':e<CR>')

    mod_bindings('telescope.builtin', function(builtin)
        nmap('<C-P>', function() builtin.find_files() end)
        nmap('<C-G>', function() builtin.git_files() end)

        -- dotfiles
        nmap('<leader>.', function() builtin.file_browser() end)
        -- [g]rep;
        nmap('<leader>fg', function() builtin.live_grep() end)

        --> [s]earch
        -- [l]ocal; [t]ags
        nmap('<leader>sl', function() builtin.current_buffer_fuzzy_find() end)
        nmap('<leader>st', function() builtin.current_buffer_tags() end)
    end)

    nmap('<leader>,', '<C-^>') -- previous file

    --> [t]ransform
    -- [w]hitespaces; [r]etab;
    nmap('<leader>tw', ':StripTrailingWhitespace<CR>')
    nmap('<leader>tr', ':retab<CR>')

    --> [p]aste
    -- [t]oggle;
    nmap('<leader>pt', ':set paste!<CR>')

    --> [c]omment
    nmap('<C-_>', ':CommentToggle<CR>')
    vmap('<C-_>', ':<C-U>call CommentOperator(visualmode())<CR>')

    --> Splits, tabs, and buffers navigation
    for _,v in ipairs({'J', 'K', 'L', 'H'}) do
        nmap('<C-'..v..'>', '<C-W><C-'..v..'>')
    end
    -- 's' stands for both search and split
    -- may sound confusing but it's fine
    nmap('<leader>sp', ':tabp<CR>')
    nmap('<leader>sn', ':tabn<CR>')

    --> Quality of Life
    nmap('<leader>d', '"_d') -- delete without yanking
    nmap('<leader>D', '"_dd') -- delete line without yanking
    nmap('<CR>', ':nohl<CR>')

    -- go up/down, clear line and autoindent
    nmap('<leader>k', 'kcc')
    nmap('<leader>j', 'jcc')

    --> Completion
    imap('<C-space>', function() vim.fn['compe#complete']() end)
    imap('<CR>', "compe#confirm(lexima#expand('<LT>CR>', 'i'))", { expr = true })
    imap('<C-K>', '<C-P>')
    imap('<C-J>', '<C-N>')

    --> LSP
    if opts.lsp then
        local lspbuf = vim.lsp.buf

        -- Goto
        nmap('gd', function() lspbuf.definition() end)
        nmap('gD', function() lspbuf.declaration() end)
        nmap('gi', function() lspbuf.implementation() end)
        nmap('gr', function() lspbuf.references() end)
        nmap('gy', function() lspbuf.type_definition() end)

        mod_bindings('lspsaga.provider', function(provider)
            nmap('<leader>gd', function() provider.preview_definition() end)
            nmap('<leader>gr', function() provider.lsp_finder() end)
        end)

        -- Search
        mod_bindings('telescope.builtin', function(builtin)
            -- [d]ocument; [w]orkspace;
            nmap('<leader>sd', function() builtin.lsp_document_symbols() end)
            nmap('<leader>sw', function() builtin.lsp_workspace_symbols() end)
            -- [r]eferences;
            nmap('<leader>sr', function() builtin.lsp_references() end)
            -- [D]efinitions;
            nmap('<leader>sD', function() builtin.lsp_definitions() end)
        end)

        -- Help
        nmap('K', function() require('lspsaga.hover').render_hover_doc() end)
        imap('<C-P>', function() require('lspsaga.signaturehelp').signature_help() end)

        -- Diagnostics
        mod_bindings('lspsaga.diagnostic', function(diagnostic)
            nmap('[d', function() diagnostic.lsp_jump_diagnostic_prev() end)
            nmap(']d', function() diagnostic.lsp_jump_diagnostic_next() end)
            nmap('<leader>dl', function() diagnostic.show_line_diagnostics() end)
            nmap('<leader>dh', function() diagnostic.show_cursor_diagnostics() end)
        end)

        -- Refactor
        nmap('<leader>rn', function() require('lspsaga.rename').rename() end)
        nmap('<leader>ri', '<Nop>') -- Initially, this is refactor (organize) imports but this is yet to be done
                                    -- tho I have a muscle memory of it so binding it to Nop prevents unwanted replaces
        -- Code actions
        nmap('<leader>a', function() require('lspsaga.codeaction').code_action() end)
        vmap('<leader>a', ":<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>")
    end

    -- Float term
    nmap('<leader>to', function() require('lspsaga.floaterm').open_float_terminal() end)
    q.map('t', '<leader><C-C>', function() require('lspsaga.floaterm').close_float_terminal() end)

    --> Quick Fix
    nmap('[q', ':cp<CR>')
    nmap(']q', ':cn<CR>')
    nmap('Q', ':ccl<CR>')

    --> Config tools
    if opts.cfg_tools then
        nmap('<leader>f.', function() spyglass.dot_files() end)
        nmap('<leader>fv', function() spyglass.config_files() end)
        nmap('<leader>fV', function()
            -- TODO: use lua api to
            -- * open tab on buffer init.lua
            -- * change cwd to stdpath config for tab (is it even possible)
            local cfg_dir = vim.fn.stdpath('config')
            vim.cmd('e ' .. cfg_dir .. '/init.lua')
            vim.cmd('cd ' .. cfg_dir)
        end)
        nmap('<leader>vrl', function() -- [v]im [r]e[l]oad
            require('editor.cfg_tools').reload()
            print('nvim configuration reloaded')
        end)
        nmap('<leader>vrd', function() -- [v]im [r]eload [d]isplay
            local cfg_tools = require('editor.cfg_tools')
            cfg_tools.reload_galaxyline()
        end)
    end
end

function M.telescope_mappings()
    local actions = require('telescope.actions')

    local utils = require('telescope.utils')
    local action_state = require('telescope.actions.state')

    local delete_file = function(prompt_bufnr)
        local cwd = action_state.get_current_picker(prompt_bufnr).cwd
        local selection = action_state.get_selected_entry()

        local confirmation = vim.fn.input('Do you really wanna delete file ' .. selection.value .. '? [y/N] ')
        if string.lower(confirmation) ~= 'y' then return end

        actions.close()
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

function M.lspsaga_mappings()
    return {
        finder_action_keys = {
            open = 'o', vsplit = 'v', split = 's',
            scroll_down = '<C-F>',scroll_up = '<C-B>',
            quit = '<C-C>',
        },
        code_action_keys = {
            exec = '<CR>',
            quit = '<C-C>',
        },
        rename_action_keys = {
            exec = '<CR>',
            quit = '<C-C>',
        },
    }
end

return M


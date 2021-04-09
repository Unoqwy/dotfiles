local galaxyline = require('galaxyline')
local condition = require('galaxyline.condition')

local fileinfo = require('galaxyline.provider_fileinfo')
local vcs = require('galaxyline.provider_vcs')

local palette = require('theme.palette')
local gls = galaxyline.section

---------------------
-- General settings
---------------------
galaxyline.short_line_list = {'packer'}

--------------
-- Utilities
--------------
local function _(component)
    if not component.separator then
        component.separator = ' '
    end
    if not component.separator_highlight then
        component.separator_highlight = {palette.grey, palette.bg}
    end
    return component
end

----------------
-- Status line
----------------
local last_mode_name = "LOADING"
local mode_colors = {
    [VimModes.Normal] = palette.blue,
    [VimModes.Insert] = palette.green,
    [VimModes.Visual] = palette.purple,
    [VimModes.Replace] = palette.red,
    [VimModes.CmdLine] = palette.orange,
}

gls.left = {
    {ViMode = _{
        provider = function()
            local mode = q.vim_mode()
            if mode then
                last_mode_name = mode[2]
                local new_color = mode_colors[mode[1]]
                if new_color then
                    vim.api.nvim_command('hi GalaxyViMode guifg=' .. new_color)
                end
            end
            return ' ● ' .. last_mode_name .. ' '
        end,
        separator = '∣ ',
        highlight = {'NONE', palette.bg},
    }},
    {FileIcon = {
        provider = 'FileIcon',
        condition = condition.buffer_not_empty,
        highlight = {fileinfo.get_file_icon_color, palette.bg},
    }},
    {FileName = _{
        provider = function()
            return vim.fn.expand("%:F")
        end,
        condition = condition.buffer_not_empty,
        highlight = {palette.fg, palette.bg},
    }},
    {FileSize = _{
        provider = function()
            return vim.trim(fileinfo.get_file_size()):upper()
        end,
        condition = condition.buffer_not_empty,
        separator = ' -> ',
        highlight = {palette.grey, palette.bg},
    }},
    {LineColumn = _{
        provider = function()
            return vim.fn.line('.') .. ':' .. vim.fn.col('.')
        end,
        highlight = {palette.grey, palette.bg},
    }},
}

gls.right = {
    {FileFormat = _{
        provider = function()
            return fileinfo.get_file_format():lower()
        end,
        highlight = {palette.grey, palette.bg},
    }},
    {GitBranch = _{
        provider = function()
            return '  ' .. vcs.get_git_branch() .. ' '
        end,
        condition = condition.check_git_workspace,
        highlight = {palette.fg, palette.stand_bg},
    }},
}

---------------
-- Short line
---------------


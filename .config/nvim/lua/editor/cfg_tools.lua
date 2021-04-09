local M = {}

local function unload_packages(match, on_unload)
    for pkg,_ in pairs(package.loaded) do
        if string.match(pkg, match) then
            package.loaded[pkg] = nil
            if on_unload then
                on_unload(pkg)
            end
        end
    end
end

function M.reload()
    local scan_dir = require('plenary.scandir').scan_dir
    local search_dir = vim.fn.stdpath('config') .. '/lua'
    local search_opts = {
        depth = 1,
        respect_gitignore = false,
        add_dirs = true,
    }

    local config_modules = {}
    for _,f in ipairs(scan_dir(search_dir, search_opts)) do
        local comps = vim.split(f, '/')
        local file = comps[#comps]
        if string.match(file, '%.lua$') then
            local file_name = string.sub(file, 0, #file - 4)
            if not string.find(file_name, '%.') and not vim.tbl_contains(config_modules, file_name) then
                table.insert(config_modules, file_name)
            end
        end
    end
    config_modules['theme'] = nil

    for _,config_module in ipairs(config_modules) do
        unload_packages('^' .. config_module)
    end

    vim.cmd(':luafile ' .. vim.fn.stdpath('config') .. '/init.lua')
end

function M.reload_galaxyline()
    -- reload galaxyline config
    require('galaxyline').disable_galaxyline()
    unload_packages('^galaxyline')
    require('galaxyline').galaxyline_augroup()
    unload_packages('theme.galaxyline', require)

    -- refresh galaxyline instantly
    require("galaxyline").load_galaxyline()
end

return M


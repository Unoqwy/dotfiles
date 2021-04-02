local plugins = {}

----------
-- Setup
----------
plugins.pre_install = function()
    -- https://github.com/wbthomason/packer.nvim#bootstrapping
    local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
        vim.cmd('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
        vim.cmd('packadd packer.nvim')
    end
end

--------------------
-- Install plugins
--------------------
plugins.install = function(modules)
    local packer = require('packer')
    packer.startup(function(use)
        use('wbthomason/packer.nvim')

        if opts.time_tracking then
            use('wakatime/vim-wakatime')
        end

        for _,module in ipairs(modules) do
            if module.install_deps ~= nil then
                module.install_deps(use)
            end
        end
    end)
end

return plugins


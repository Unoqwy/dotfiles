local function pre_install()
    vim.cmd('packadd packer.nvim')
end

local function install(modules)
    local packer = require('packer')
    packer.startup(function(use)
        use('wbthomason/packer.nvim')

        use('axelf4/vim-strip-trailing-whitespace')

        if opts.time_tracking then
            use('wakatime/vim-wakatime')
        end

        for _,module in ipairs(modules) do
            if module.install_deps ~= nil then
                module.install_deps(use)
            end
        end

	-- TODO: auto sync
    end)
end

return { pre_install = pre_install, install = install }


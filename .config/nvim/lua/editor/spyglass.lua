local M = {}

function M.dot_files()
    require('telescope.builtin').find_files {
        find_command = {q.ext_dir .. '/dotfiles', 'ls-tree', '--full-tree', '-r', '--name-only', 'HEAD'},
        cwd = vim.fn.expand('$HOME'),
    }
end

function M.config_files()
    require('telescope.builtin').find_files {
        cwd = vim.fn.stdpath('config'),
    }
end

return M


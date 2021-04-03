--------------
-- Utilities
--------------
local function bake_q(scopes)
    return setmetatable({}, {
        __newindex = function(_, key, value)
            for _,scope in ipairs(scopes) do
                scope[key] = value
            end
        end,
    })
end

_G.q.o = bake_q {vim.o}
_G.q.wo = bake_q {vim.wo, vim.o}
_G.q.bo = bake_q {vim.bo, vim.o}

-----------------
-- Set settings
-----------------
--> Global
q.o.smarttab = true
q.o.showmode = false
q.o.ignorecase = true
q.o.smartcase = true

q.o.hidden = true
q.o.splitbelow = true
q.o.splitright = true

--> Window
q.wo.number = true
q.wo.relativenumber = true
q.wo.signcolumn = "yes:1"
q.wo.list = true

q.wo.wrap = true
q.wo.linebreak = true

--> Buffer
q.bo.expandtab = true
q.bo.tabstop = opts.indent_level
q.bo.shiftwidth = opts.indent_level

q.bo.autoindent = true
q.bo.smartindent = true


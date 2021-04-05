--------------
-- Utilities
--------------
--> Missing lua ops
function table.includes(tbl, elt)
    for i,val in ipairs(tbl) do
        if val == elt then
            return true
        end
    end
    return false
end

--> Vim Mode
_G.VimModes = {
    Normal = 1,
    Insert = 2,
    Visual = 3,
    Select = 3,
    Replace = 4,
    CmdLine = 5,
    External= 7,
}

local modes = {
    n = {VimModes.Normal, "NORMAL"},
    no = {VimModes.Normal, "NORMAL~"},
    i = {VimModes.Insert, "INSERT"},
    ic = {VimModes.Insert, "INSERT~"},
    v = {VimModes.Visual, "VISUAL"},
    V = {VimModes.Visual, "V-LINE"},
    [''] = {VimModes.Visual, "V-BLOCK"},
    s = {VimModes.Select, "SELECT"},
    S = {VimModes.Select, "S-LINE"},
    [''] = {VimModes.Select, "S-BLOCK"},
    R = {VimModes.Replace, "REPLACE"},
    Rv = {VimModes.Replace, "REPLACE~"},
    c = {VimModes.CmdLine, "COMMAND"},
    t = {VimModes.External, "TERMINAL"},
}

function q.vim_mode()
    local mode = modes[vim.fn.mode()]
    if mode then
        return mode
    end
end

--> Error handling
function q.safe_call(fn)
    local status, err = pcall(fn)
    if not status then
        print('[ERROR]\n' .. err .. '\n[/ERROR]')
    end
end


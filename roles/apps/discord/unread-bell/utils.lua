local M = {}

function M.tbl_contains(tbl, value)
    for _, val in ipairs(tbl) do
        if value == val then
            return true
        end
    end
    return false
end

function M.append_str(str, val)
    if str ~= nil then
        return str .. " " .. val
    else
        return val
    end
end

return M

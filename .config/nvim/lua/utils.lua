--------------
-- Utilities
--------------
function table.includes(tbl, elt)
    for i,val in ipairs(tbl) do
        if val == elt then
            return true
        end
    end
    return false
end


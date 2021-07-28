---@type UnreadBell
local unread_bell = require("unread-bell")

local utils = require("utils")
local local_config = require("local_config")

local M = {}

local fifo = io.popen("cat > /tmp/discord.pipe", "w")
fifo:write("\n")
fifo:flush()

local function colored(str, color)
    return "<fc=" .. color  .. ">" .. str .. "</fc>"
end

local function icon(str)
    return "<fn=2>" .. str .. "</fn>"
end

---@param revive boolean
function M.on_update(revive)
    local work_bell = false
    local spotlight = {}
    local other_mentions_count = 0

    local notifications = unread_bell.get_notifications()
    for user, info in pairs(notifications.dms) do
        if local_config.colored_buddies ~= nil and local_config.colored_buddies[user] ~= nil then
            table.insert(spotlight, colored(icon("\u{f755}"), local_config.colored_buddies[user]))
        elseif local_config.work_dms ~= nil and utils.tbl_contains(local_config.work_dms, user) then
            work_bell = true
        else
            other_mentions_count = other_mentions_count + info.unread_count
        end
    end
    for group, info in pairs(notifications.groups) do
        if local_config.colored_groups ~= nil and local_config.colored_groups[group] ~= nil then
            table.insert(spotlight, colored(icon("\u{f186}"), local_config.colored_groups[group]))
        elseif local_config.work_dms ~= nil and utils.tbl_contains(local_config.work_dms, group) then
            work_bell = true
        else
            other_mentions_count = other_mentions_count + info.unread_count
        end
    end
    for _, info in pairs(notifications.guilds) do
        other_mentions_count = other_mentions_count + info.mention_count
    end

    local output
    if work_bell then
        output = utils.append_str(output, colored(icon("\u{f77f}"), "#FF5722"))
    end
    if #spotlight > 0 then
        output = utils.append_str(output, table.concat(spotlight, " "))
    end
    if other_mentions_count > 0 then
        output = utils.append_str(output, colored("[+" .. other_mentions_count .. "]", "#948ae3"))
    end

    fifo:write(output ~= nil and output .. "\n" or "\n")
    fifo:flush()
end

function M.on_close()
    fifo:write("\n")
    fifo:close()
end

return M

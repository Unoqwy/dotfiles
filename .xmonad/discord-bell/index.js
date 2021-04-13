const { readFileSync, createWriteStream } = require('fs');
const { join } = require('path');

const libNotif = require('./discord-notifications');

var configFile, config;
var plugged = false, checkTask;
var wstream, statusLine, revive;

function check() {
    if (wstream === undefined) {
        return;
    }

    let icon = config.icons.idle, colored = false, customColor = null;
    if (libNotif.getBadgeCount(['GUILD', 'FRIEND_REQUEST']) > 0) {
        if (config.icons.mention) {
            icon = config.icons.mention;
        }
        colored = true;
    }

    let dms = Object.keys(libNotif.getDMList());
    if (dms.length > 0) {
        if (config.icons.dm) {
            icon = config.icons.dm;
        }
        colored = true;

        for (var userId in config.coloredDMs) {
            if (dms.includes(userId)) {
                customColor = config.coloredDMs[userId];
            }
        }
    }

    let color = (customColor !== null ?
        customColor : (colored && config.colors.colored ?
            config.colors.colored : config.colors.default
        )
    );

    var line;
    if (icon) {
        line = [
            //"<box type=Bottom width=2" + (color ? " color=" + color : "") + ">",
            color ? "<fc=" + color + ">" : "",
            "<fn=2>", icon, "</fn>",
            color ? "</fc>" : "",
            //"</box>",
            "\n"
        ].join("");
    } else {
        line = "\n";
    }

    if (revive >= 10 || statusLine !== line) {
        statusLine = line;
        try {
            wstream.write(line);
        } catch (_) {
            // writestream has been destroyed (xmobar most likely restarted)
            wstream = createWriteStream(config.pipePath);
        }
        revive = 0;
    } else {
        revive++;
    }
}

module.exports = {
    // give access to libNotif inside devtools for some tests
    libNotif: libNotif,

    reloadConfig: function () {
        config = JSON.parse(readFileSync(configFile, 'utf8'));

        if (wstream) {
            wstream.end();
        }
        wstream = createWriteStream(config.pipePath);
        return true;
    },
    plug: function(cfgFile) {
        if (plugged) return;

        configFile = cfgFile;
        this.reloadConfig();
        setTimeout(check, 5000);
        checkTask = setInterval(check, 10000);

        plugged = true;
        return true;
    },
    unplug: function() {
        if (!plugged) return;

        try {
            wstream.end();
        } catch (_) {}
        if (checkTask !== undefined) {
            clearInterval(checkTask);
        }
        plugged = false;
        return true;
    }
}


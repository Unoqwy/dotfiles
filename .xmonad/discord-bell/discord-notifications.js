/* This short lib can retrieve notication count and
 * who's sent you a DM inside the official Discord electron app
 * when you can't access flux state
 *
 * (in this case: when injecting with "exposeInMainWorld", it can't
 *  access any "regular" js therefore I can't extract from flux's state
 *  but instead I need to read from the document)
 */

module.exports = {
    BADGE_TYPE: {
        ALL: 1 << 0,
        DM: 1 << 1,
        GUILD: 1 << 2,
        FRIEND_REQUEST: 1 << 3,
    },
    BADGE_TYPE_SELECTORS: {
        DM: "div:not([class^='tutorialContainer']) > [class^='listItem'] > [class^='listItemWrapper'] [class^='numberBadge']",
        FRIEND_REQUEST: "[class^='tutorialContainer'] [class^='numberBadge']",
        GUILD: "[aria-label='Servers'] [class^='numberBadge']",
    },

    /**
     * Gets notification count by reading badges
     * @param {(number|string)} bits - Bit representation or string array of chosen badge types
     * @returns {number}
     */
    getBadgeCount: function(bits) {
        if (typeof bits != "number") {
            var _tmpBits = 0;
            for (var badgeType of bits) {
                _tmpBits |= this.BADGE_TYPE[badgeType.toUpperCase()];
            }
            bits = _tmpBits;
        }

        let selectors = [];
        const all = (bits & this.BADGE_TYPE.ALL) != 0;
        for (var badgeType in this.BADGE_TYPE) {
            if (badgeType == 'ALL') continue;
            if (all || (bits & this.BADGE_TYPE[badgeType]) != 0) {
                selectors.push(this.BADGE_TYPE_SELECTORS[badgeType]);
            }
        }

        if (selectors.length == 0) {
            return 0;
        }
        return [0, ...[...document.querySelectorAll(selectors.join(', '))]
            .map(elt => parseInt(elt.innerText))]
            .reduce((a, b) => a + b);
    },

    /**
     * Gets DM notifications with their senders
     * @returns {object}
     */
    getDMList: function() {
        let dmList = {};
        let dmNodes = document.querySelectorAll("div:not([class^='tutorialContainer']) > [class^='listItem'] > [class^='listItemWrapper']");
        for (var dm of dmNodes) {
            var numberBadge = dm.querySelector("[class^='numberBadge']");
            if (!numberBadge) continue;

            var number = parseInt(numberBadge.innerText);
            var innerHTML = dm.innerHTML.toString();
            var userMatch = innerHTML.match(/avatars\/(\d+)/);
            if (userMatch !== null) {
                dmList[userMatch[1]] = number;
            } else {
                // dm is group
                var groupId = 'G:' + innerHTML.match(/channels\/@me\/(\d+)/)[1];
                dmList[groupId] = number;
            }
        }
        return dmList;
    },
 }


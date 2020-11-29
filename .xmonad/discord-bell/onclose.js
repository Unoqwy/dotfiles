// script to hide bell from bar when Discord closes

const { createWriteStream } = require('fs');
const { join } = require('path');
const { pipePath } = require(join(__dirname, 'config.json'));

var wstream = createWriteStream(pipePath);
wstream.write("<fn=0></fn>\n");
wstream.end();


/* global BROWSER_RUNTIME */

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("/tmp/script.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "/tmp/script.plutus"),
    "utf8"
  );
}
console.log("LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL")
console.log(script)

exports.spamScript = script;

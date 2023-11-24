lib = require("@emurgo/cardano-serialization-lib-nodejs");
exports.getPrivateKeyFromHex = hex => lib.PrivateKey.from_hex(hex)

lib = require("@emurgo/cardano-serialization-lib-nodejs");
exports.getPrivateKeyFromHex = hex => lib.PrivateKey.from_hex(hex)
exports.genPrivateKey= () => lib.PrivateKey.generate_ed25519()
exports.getPrivateKeyHex = pkey => pkey.to_hex()
exports.getPubKeyHex = pkey => pkey.to_public().to_hex()

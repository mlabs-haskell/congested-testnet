lib = require("@emurgo/cardano-serialization-lib-nodejs");
exports.getPrivateKeyFromHex = hex => lib.PrivateKey.from_hex(hex)
exports.genPrivateKey= () => lib.PrivateKey.generate_ed25519()
exports.getPrivateKeyHex = pkey => pkey.to_hex()
exports.getPubKeyHex = pkey => pkey.to_public().to_hex()
exports.getEd25519HashFromPubKeyHex = hex => lib.PublicKey.from_hex(hex).hash()
exports.getPubKeyHashHex = pkey => pkey.to_public().hash().to_hex()
exports.getEdHash = pkey => pkey.to_public().hash()
exports.getHexFromEd25519Hash = ed => ed.to_hex()

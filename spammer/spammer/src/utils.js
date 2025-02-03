const csl = require("@emurgo/cardano-serialization-lib-nodejs");

const generatePkeys = N => {
  var pkeys = [];

  for (var i = 1; i <= N; i++) {
    pkeyHex = csl.PrivateKey.generate_ed25519().to_hex();
    pkeys.push(pkeyHex);
  };

  return pkeys;
};

const saveKeys = pkeys => fname => {
  const fs = require("fs");
  fs.writeFileSync(fname, JSON.stringify(pkeys));
};

const uploadKeys = fname => {
  const fs = require("fs");
  const bechs = JSON.parse(fs.readFileSync(fname, 'utf8'));
  return bechs 
};

const hash = pkeyHex => csl.PrivateKey.from_hex(pkeyHex).to_public().hash().to_hex();

module.exports = {
  hash,
  generatePkeys,
  saveKeys,
  uploadKeys
};


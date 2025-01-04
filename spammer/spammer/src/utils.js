const generatePkeys = N => {
  const csl  = require("@emurgo/cardano-serialization-lib-nodejs");
  var pkeys = [];

  for (var i = 1; i <= N; i++) {
    hex =csl.PrivateKey.generate_ed25519().to_hex();
    pkeys.push(hex);
  };

  return pkeys;

};

const saveKeys = pkeys => fname => {
  const fs = require("fs");
  fs.writeFileSync(fname, JSON.stringify(pkeys))
};

const uploadKeys = fname => {
  const fs = require("fs");
  return JSON.parse(fs.readFileSync(fname, 'utf8'));
};

module.exports = {
  generatePkeys,
  saveKeys,
  uploadKeys
};


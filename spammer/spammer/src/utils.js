const csl = require("@emurgo/cardano-serialization-lib-nodejs");

const generatePkeys = N => {
  var pkeys = [];

  for (var i = 1; i <= N; i++) {
    hex = csl.PrivateKey.generate_ed25519();
    pkeys.push(hex);
  };

  return pkeys;

};

const saveKeys = pkeys => fname => {
  const fs = require("fs");
  fs.writeFileSync(fname, JSON.stringify(pkeys.map(pk => pk.to_hex())));
};

const uploadKeys = fname => {
  const fs = require("fs");
  const hexs = JSON.parse(fs.readFileSync(fname, 'utf8'));
  return hexs.map(h => csl.PrivateKey.from_hex(h));
};

const hash = pkey => pkey.to_public().hash();

module.exports = {
  hash,
  generatePkeys,
  saveKeys,
  uploadKeys
};


const csl = await import("@emurgo/cardano-serialization-lib-nodejs");

export const generatePkeys = N => {
  var pkeys = [];

  for (var i = 1; i <= N; i++) {
    pkey_bech = csl.PrivateKey.generate_ed25519().to_hex();
    pkeys.push(pkey_bech);
  };

  return pkeys;
};

export const saveKeys = pkeys => fname => {
  const fs = require("fs");
  fs.writeFileSync(fname, JSON.stringify(pkeys));
};

export const uploadKeys = fname => {
  const fs = require("fs");
  const bechs = JSON.parse(fs.readFileSync(fname, 'utf8'));
  return bechs 
};

export const hash = pkey_hex => csl.PrivateKey.from_hex(pkey_hex).to_public().hash().to_hex();

// module.exports = {
//   hash,
//   generatePkeys,
//   saveKeys,
//   uploadKeys
// };


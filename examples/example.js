/** before install necessary packages 
 npm install @emurgo/cardano-serialization-lib-nodejs
 npm install node-fetch 
*/

// example transaction using cardano-serialization-lib and congested testnet server
(async () => { 
  const csl = await import("@emurgo/cardano-serialization-lib-nodejs");
  // generate cardano private payment key
  const pkey = csl.PrivateKey.generate_ed25519();
  // TODO delete this line
  console.log(pkey.to_hex())
  const pubKeyHashHex = pkey.to_public().hash().to_hex();
  console.log(`generated wallet pubKeyHashHex: ${pubKeyHashHex}`)

  // request 1000 tada from faucet
  console.log(`request 1000 tada from faucet`)
  let txHash = await get1000tada(pubKeyHashHex,'http://0.0.0.0:8000');
  let time  = await awaitTxTime(txHash);
  console.log(`${txHash} added to block after ${time} seconds`)
  const pk = "0a7c59147b1576f3c6bc4d1160b7d41178969ac3ca275ddc52493fb5b2d6d764";
  // txHash = await simpleTxWithOgmios(pk); 
  // time  = await awaitTxTime(txHash);
})()



const simpleTxWithOgmios = (pkey) => {

};


const get1000tada = async (pubKeyHashHex, faucetUrl) => {
  const fetch = await import('node-fetch');
  const faucetResponse = await fetch.default(`${faucetUrl}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json'},
      body: JSON.stringify({pubKeyHashHex : pubKeyHashHex})
  });
  const data = await faucetResponse.json();
  console.log(data);
  return data.message.txHash;
}; 


// request awaitTxTime with kupo
const awaitTxTime = async txHash => {
    console.log(`wait until ${txHash} added to block ...`)
    const fetch = await import("node-fetch");
    const url = `http://0.0.0.0:1442/matches/*@${txHash}`;
    const start = Date.now();
    while (true) {
      try { 
        const resp = await fetch.default(url);
        const body = await resp.json();
        if (body.length > 0) return (Date.now() - start)/1000;
      } catch (err) { 
        throw Error ("kupo requests error") 
      }
      await new Promise((resolve) => setTimeout(() => resolve(), 4000))
    }
};


// const buildTransaction = async () => {
//   // this part taken from 
//   const csl = await import("@emurgo/cardano-serialization-lib-nodejs");
//   const txBuilder = csl.TransactionBuilder.new(
//     csl.TransactionBuilderConfigBuilder.new()
//       .fee_algo(csl.LinearFee.new(csl.BigNum.from_str('44'), csl.BigNum.from_str('155381')))
//       .coins_per_utxo_word(wasm.BigNum.from_str('34482'))
//       .pool_deposit(wasm.BigNum.from_str('500000000'))
//       .key_deposit(wasm.BigNum.from_str('2000000'))
//       .ex_unit_prices(
//         wasm.ExUnitPrices.new(
//           wasm.UnitInterval.new(wasm.BigNum.from_str('577'), wasm.BigNum.from_str('10000')),
//           wasm.UnitInterval.new(wasm.BigNum.from_str('721'), wasm.BigNum.from_str('10000000')),
//         ),
//       )
//       .max_value_size(5000)
//       .max_tx_size(16384)
//       .build(),
//   )
// }

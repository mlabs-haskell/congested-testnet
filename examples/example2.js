
// main function 
(async () => {
  const {execSync} = await import("child_process");
  const {writeFileSync ,readFileSync} = await import("fs");
  const csl = await import("@emurgo/cardano-serialization-lib-nodejs");
  let result;

  // generate keys 
  const vkey = "key.vkey";
  const skey = "key.skey";
  // execSync(`cardano-cli conway address key-gen \\
  //   --verification-key-file ${vkey} \\
  //   --signing-key-file ${skey}`
  // );
  // derive address
  const addr = "wallet.addr"
  result = execSync(
      `cardano-cli conway address build \\
        --payment-verification-key-file ${vkey} \\
        --out-file ${addr} \\
        --testnet-magic 42` 
  );
  // derive pubKeyHashHex
  result = execSync(
      `cardano-cli conway address key-hash \\
        --payment-verification-key-file ${vkey}` 
  );
  let pubKeyHashHex = result.toString().replace(/\n/g, ''); 

  let txHash = await get1000tada(pubKeyHashHex,'http://0.0.0.0:8000');
  let time  = await awaitTxTime(txHash);
  console.log(`${txHash} added to block after ${time} seconds`)
  writeFileSync("txHash", txHash);
  
})()



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

  // console.log(pubKeyHashHex);
  // result = readFileSync(vkey);
  // result = JSON.parse(result.toString());
  // vvkey = result.cborHex.slice(4);
  // console.log(vvkey);
  // console.log(csl.PublicKey.from_hex(vvkey).hash().to_hex())

  // request 1000 tada from faucet
  // console.log(`request 1000 tada from faucet`)

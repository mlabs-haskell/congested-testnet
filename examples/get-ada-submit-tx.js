// main function 
(async () => {
  const {execSync} = await import("child_process");
  const {readFileSync} = await import("fs");
  let result;

  // generate keys 
  const vkey = "key.vkey";
  const skey = "key.skey";
  execSync(`cardano-cli conway address key-gen \\
    --verification-key-file ${vkey} \\
    --signing-key-file ${skey}`
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
  // writeFileSync("txHash", txHash);
  // let txHash = readFileSync("txHash").toString(); 

  // derive address
  const addr = execSync(
      `cardano-cli conway address build \\
        --payment-verification-key-file ${vkey} \\
        --testnet-magic 42` 
  ).toString();
  console.log(addr)

  // request utxos with kupo
  const utxos = await requestKupoUtxos(addr);
  console.log(utxos)
  const txid = utxos[0].transaction_id;
  console.log(txid);


  let fee = 100000

  execSync(
    `cardano-cli conway transaction build-raw \
        --tx-in ${txid}#0 \
        --tx-out ${addr}+3000000 \
        --tx-out ${addr}+${10000000000 - 3000000 - fee} \
        --fee ${fee} \
        --out-file tx.raw`
  )

  fee = execSync(
    `cardano-cli conway transaction calculate-min-fee \
      --tx-body-file tx.raw \
      --witness-count 1 \
      --protocol-params-file ../temp.json`
  )

  // fee in lovelace
  fee = fee.toString().split(" ")[0]
  console.log(fee)

  // rebuild transaction
  execSync(
    `cardano-cli conway transaction build-raw \
        --tx-in ${txid}#0 \
        --tx-out ${addr}+3000000 \
        --tx-out ${addr}+${10000000000 - 3000000 - fee} \
        --fee ${fee} \
        --out-file tx.raw`)

  // sign transaction
  execSync(
    `cardano-cli conway transaction sign \
        --tx-body-file tx.raw \
        --signing-key-file key.skey \
        --out-file tx.signed`)

  // submit TX
  txSigned = JSON.parse(readFileSync('tx.signed'));
  console.log(txSigned.cborHex)

  let submitResponse = await requestOgmios("submitTransaction", { transaction : {cbor : txSigned.cborHex}});
  console.log(submitResponse)

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

//request utxos with kupo
const requestKupoUtxos = async addr => {
    console.log(`request utxos with kupo for ${addr}`)
    const fetch = await import("node-fetch");
    const url = `http://0.0.0.0:1442/matches/${addr}`;
    const resp = await fetch.default(url);
    const body = await resp.json();
    return body
};

const requestOgmios = async (method, params) => {
  const fetch = await import("node-fetch");
   const url = `http://0.0.0.0:1337`;
    const resp = await fetch.default(url, {
      method: "POST",
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method,
        params : params})
    });
    const body = await resp.json();
    return body

};


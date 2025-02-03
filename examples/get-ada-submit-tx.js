// In this example we use cardano-cli 10.1.1.0 - linux-x86_64 - ghc-8.10 git rev 1f63dbf2ab39e0b32bf6901dc203866d3e37de08
// This demonstrates how to get ADA using the faucet. 
// How to build a simple transaction using cardano-cli, ogmios and kupo 

(async () => {
  const url = `http://congested-testnet.staging.mlabs.city`
  // const url = `http://0.0.0.0`
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

  let txHash = await get1000tada(`${url}:8000`, pubKeyHashHex);
  let time  = await awaitTxTimeWithKupo(`${url}:1442`, txHash);
  console.log(`${txHash} added to block after ${time} seconds`)

  // derive address
  const addr = execSync(
      `cardano-cli conway address build \\
        --payment-verification-key-file ${vkey} \\
        --testnet-magic 42` 
  ).toString();

  // request utxos with kupo
  const utxos = await requestKupoUtxos(`${url}:1442`, addr);
  console.log(`utxos:`)
  console.log(utxos)
  const txid = utxos[0].transaction_id;
  console.log(`take utxo ${txid} for simple pay transaction`);


  let fee = 100000

  execSync(
    `cardano-cli conway transaction build-raw \
        --tx-in ${txid}#0 \
        --tx-out ${addr}+3000000 \
        --tx-out ${addr}+${10000000000 - 3000000 - fee} \
        --fee ${fee} \
        --out-file tx.raw`
  )

  // download protocol parameters
  await downloadFile(`${url}:5000/protocol.json`,`protocol.json`)

  // correct fee 
  fee = execSync(
    `cardano-cli conway transaction calculate-min-fee \
      --tx-body-file tx.raw \
      --witness-count 1 \
      --protocol-params-file protocol.json`
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
  let submitResponse = await requestOgmios(`${url}:1337`, "submitTransaction", { transaction : {cbor : txSigned.cborHex}});
  console.log(submitResponse)
  console.log(`tx is submitted`)
  time  = await awaitTxTimeWithKupo(`${url}:1442`, submitResponse.result.transaction.id);
  console.log(`${txHash} added to block after ${time} seconds`)

})()



const get1000tada = async (url, pubKeyHashHex) => {
  const fetch = await import('node-fetch');
  console.log(url);
  const faucetResponse = await fetch.default(url, {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({pubKeyHashHex : pubKeyHashHex})
  });
  const data = await faucetResponse.json();
  console.log(data);
  return data.message.txHash;
}; 


// request awaitTxTime with kupo
const awaitTxTimeWithKupo = async (url, txHash) => {
    console.log(`wait until ${txHash} added to block ...`)
    const fetch = await import("node-fetch");
    const start = Date.now();
    while (true) {
      try { 
        const resp = await fetch.default(`${url}/matches/*@${txHash}`);
        const body = await resp.json();
        if (body.length > 0) return (Date.now() - start)/1000;
      } catch (err) { 
        throw Error ("kupo requests error") 
      }
      await new Promise((resolve) => setTimeout(() => resolve(), 4000))
    }
};

//request utxos with kupo
const requestKupoUtxos = async (url, addr) => {
    console.log(`request utxos with kupo for ${addr}`)
    const fetch = await import("node-fetch");
    const resp = await fetch.default(`${url}/matches/${addr}`);
    const body = await resp.json();
    return body
};

// ogmios interface  
const requestOgmios = async (url, method, params) => {
  const fetch = await import("node-fetch");
    const resp = await fetch.default(`${url}`, {
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


const downloadFile = async (url, fpath) => {
  const fs = await import('fs');
  const fetch = await import ('node-fetch');
  console.log(`download file from ${url} to ${fpath}`)

  return new Promise(resolve => { 
    fetch.default(url)
        .then(res => {
            const fileStream = fs.createWriteStream(fpath);
            res.body.pipe(fileStream);
            res.body.on('end', () => console.log(`download to ${fpath} completed!`));
            resolve()
        })
        .catch(err => console.error('error download file:', err));
  })
};


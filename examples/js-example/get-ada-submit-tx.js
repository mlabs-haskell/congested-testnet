import fs from 'fs';
import fetch from 'node-fetch';
import {execSync} from 'child_process';


main().catch(console.error);

function cardano_cli(...args) {
  // Get absolute path for proper volume mounting
  const currentDir = process.cwd();
  
  // Add user permissions to ensure Docker can write files
  const userId = execSync('id -u').toString().trim();
  const groupId = execSync('id -g').toString().trim();
  
  const command = `docker run --rm -v ${currentDir}:/workspace --user ${userId}:${groupId} --entrypoint /bin/cardano-cli ghcr.io/intersectmbo/cardano-node:10.4.1 ${args.join(' ')}`;
  console.log(`Executing: ${command}`);
  try {
    const result = execSync(command, { encoding: 'utf8' });
    return result.trim();
  } catch (error) {
    console.error(`Error executing cardano-cli: ${error.message}`);
    throw error;
  }
}

async function main() {
  const dataDir = "data";
  if (!fs.existsSync(dataDir)) {
    fs.mkdirSync(dataDir);
  }
  
  const vkeyFile = `${dataDir}/key.vkey`;
  const skeyFile = `${dataDir}/key.skey`;
  const faucetTxHashFile = `${dataDir}/faucet-txHash.txt`;
  const protocolParamsFile = `${dataDir}/protocol.json`;
  const txRawFile = `${dataDir}/tx.raw`;
  const txSignedFile = `${dataDir}/tx.signed`;

  const url = `http://localhost`

  await downloadFile(`${url}:5000/protocol.json`, protocolParamsFile)

  // Request funds from the faucet.
  // Make sure data directory exists inside Docker container
  console.log(`Generating keys in ${dataDir}...`);
  cardano_cli('conway', 'address', 'key-gen',
    `--verification-key-file /workspace/${vkeyFile}`,
    `--signing-key-file /workspace/${skeyFile}`
  );
  console.log(`Key generation command completed.`);

  // Check if key files exist before proceeding
  if (!fs.existsSync(vkeyFile) || !fs.existsSync(skeyFile)) {
    console.error(`Key files not found at ${vkeyFile} or ${skeyFile}. Please check the path.`);
    process.exit(1);
  }

  // derive pubKeyHashHex
  const pubKeyHashHex = cardano_cli('conway', 'address', 'key-hash',
    `--payment-verification-key-file /workspace/${vkeyFile}`
  ); 

  const faucetTxHash = await get1000tada(`${url}:8000`, pubKeyHashHex);
  fs.writeFileSync(faucetTxHashFile, faucetTxHash);

  const faucetTxTime = await awaitTxTimeWithKupo(`${url}:1442`, faucetTxHash);
  
  console.log(`$Faucet tx ${faucetTxHash} added to block after ${faucetTxTime} seconds.`);

  // derive address
  const addr = cardano_cli('conway', 'address', 'build',
    `--payment-verification-key-file /workspace/${vkeyFile}`,
    `--testnet-magic 42`
  );

  // request utxos with kupo
  const utxos = await requestKupoUtxos(`${url}:1442`, addr);
  console.log(`utxos:`)
  console.log(utxos)
  const inputTxId = utxos[0].transaction_id;
  const inputLovelace = utxos[0].value.coins;
  console.log(`take utxo with txId '${inputTxId}' and ${inputLovelace} lovelace for simple pay transaction`);

  let fee = 100000;

  cardano_cli('conway', 'transaction', 'build-raw',
    `--tx-in ${inputTxId}#0`,
    `--tx-out ${addr}+3000000`,
    `--tx-out ${addr}+${inputLovelace - 3000000 - fee}`,
    `--fee ${fee}`,
    `--out-file /workspace/${txRawFile}`
  )

  // download protocol parameters
  await downloadFile(`${url}:5000/protocol.json`, protocolParamsFile)

  // correct fee 
  fee = cardano_cli('conway', 'transaction', 'calculate-min-fee',
    `--tx-body-file /workspace/${txRawFile}`,
    `--witness-count 1`,
    `--protocol-params-file /workspace/${protocolParamsFile}`
  )

  // fee in lovelace
  fee = fee.toString().split(" ")[0]
  console.log(fee)

  // rebuild transaction
  cardano_cli('conway', 'transaction', 'build-raw',
    `--tx-in ${inputTxId}#0`,
    `--tx-out ${addr}+3000000`,
    `--tx-out ${addr}+${10000000000 - 3000000 - fee}`,
    `--fee ${fee}`,
    `--out-file /workspace/${txRawFile}`
  )

  // sign transaction
  cardano_cli('conway', 'transaction', 'sign',
    `--tx-body-file /workspace/${txRawFile}`,
    `--signing-key-file /workspace/${skeyFile}`,
    `--out-file /workspace/${txSignedFile}`
  )

  // submit TX
  const txSigned = JSON.parse(fs.readFileSync(txSignedFile));
  const submitResponse = await requestOgmios(`${url}:1337`, "submitTransaction", { transaction : {cbor : txSigned.cborHex}});
  console.log(submitResponse)
  console.log(`tx is submitted`)
  const txTime  = await awaitTxTimeWithKupo(`${url}:1442`, submitResponse.result.transaction.id);
  console.log(`${submitResponse.result.transaction.id} added to block after ${txTime} seconds`)

};

async function get1000tada(url, pubKeyHashHex) {
  console.log(`request tada from ${url} for ${pubKeyHashHex}`);
  const faucetResponse = await fetch(url, {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({pubKeyHashHex : pubKeyHashHex})
  });
  console.log(`faucet response: ${faucetResponse.status} ${faucetResponse.statusText}`);
  const data = await faucetResponse.json();
  console.log(data);
  return data.message.txHash;
}; 


// request awaitTxTime with kupo
async function awaitTxTimeWithKupo(url, txHash) {
    console.log(`wait until ${txHash} added to block ...`)
    const start = Date.now();
    while (true) {
      try { 
        const resp = await fetch(`${url}/matches/*@${txHash}`);
        const body = await resp.json();
        if (body.length > 0) return (Date.now() - start)/1000;
      } catch (err) { 
        throw Error ("kupo requests error") 
      }
      await new Promise((resolve) => setTimeout(() => resolve(), 4000))
    }
};

//request utxos with kupo
async function requestKupoUtxos(url, addr) {
    console.log(`request utxos with kupo for ${addr}`)
    const resp = await fetch(`${url}/matches/${addr}`);
    const body = await resp.json();
    return body
};

// ogmios interface  
async function requestOgmios(url, method, params) {
    const resp = await fetch(`${url}`, {
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


function downloadFile(url, fpath) {
  console.log(`download file from ${url} to ${fpath}`);

  return new Promise((resolve, reject) => {
    fetch(url)
      .then(res => {
        if (!res.ok) {
          throw new Error(`HTTP error! status: ${res.status}`);
        }
        const fileStream = fs.createWriteStream(fpath);
        
        fileStream.on('error', error => {
          reject(error);
        });

        res.body.on('error', error => {
          fileStream.destroy();
          reject(error);
        });

        fileStream.on('finish', () => {
          fileStream.close();
          console.log(`download to ${fpath} completed!`);
          resolve();
        });

        res.body.pipe(fileStream);
      })
      .catch(error => {
        reject(error);
      });
  });
}


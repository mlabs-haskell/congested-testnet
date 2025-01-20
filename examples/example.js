const csl = require("@emurgo/cardano-serialization-lib-nodejs");
const fetch = require('node-fetch');


(async () => { 

// generate cardano private payment key
const pkey = csl.PrivateKey.generate_ed25519();

// request 1000 tada from faucet
const postData = {
  pubKeyHashHex : pkey.to_public().hash().to_hex() 
};

const faucetResponse = await fetch('http://0.0.0.0:8000', {
    method: 'POST',
    headers: {
        'Content-Type': 'application/json',
    },
    body: JSON.stringify(postData),
})

const data = await faucetResponse.json();
const txHash = data.message.txHash;

// await until tx submitted
const waitingTime = await waitTxIsSubmitted(txHash);

console.log(txHash);
})()



// function measure tx time
const waitTxIsSubmitted = async txHash => {
  return new Promise((resolve) => {
    const startTime = Date.now();

    const interval  = setInterval(async () => {
        // const kupoUrl = `http://congested-testnet.staging.mlabs.city:1442/matches/*@${txHash}`
        const kupoUrl = `http://0.0.0.0:1442/matches/*@${txHash}`
        const faucetResponse = await fetch(kupoUrl, {
            method: 'GET'
        });
        const data = await faucetResponse.json();
        console.log(data);
        
    }, 1000);
    });

};


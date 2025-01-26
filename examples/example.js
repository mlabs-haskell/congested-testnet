/** before install necessary packages 
 npm install @emurgo/cardano-serialization-lib-nodejs
 npm install node-fetch 
*/

// example transaction using cardano-serialization-lib and congested testnet server
(async () => { 
  const csl = await import("@emurgo/cardano-serialization-lib-nodejs");
  // generate cardano private payment key
  const pkey = csl.PrivateKey.generate_ed25519();
  const pubKeyHashHex = pkey.to_public().hash().to_hex();

  // request 1000 tada from faucet
  await get1000tada(pubKeyHashHex,'http://0.0.0.0:8000');

  // console.log(txHash);
})()


const get1000tada = async (pubKeyHashHex, faucetUrl) => {
  const fetch = await import('node-fetch');
  const faucetResponse = await fetch.default(`${faucetUrl}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json'},
      body: JSON.stringify({pubKeyHashHex : pubKeyHashHex})
  });
  const data = await faucetResponse.json();
  console.log(data)
  // const txHash = data.message.txHash;
}; 



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


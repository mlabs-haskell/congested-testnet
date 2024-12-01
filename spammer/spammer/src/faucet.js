
function waitForTxHash(obj) {
  return new Promise((resolve) => {
    const interval = setInterval(() => {
      if (obj.txHash != null) {
        clearInterval(interval);
        resolve(obj.txHash);
      }
    }, 200); 
  });
}



const faucet = async () => {
  const http = await import("http");
  const {parentPort} = await import("node:worker_threads");
  const path = await import("node:path");
  const csl  = await import("@emurgo/cardano-serialization-lib-nodejs");
  const {getFundsFromFaucet} = await import(path.resolve(__dirname, "../output/Spammer/index.js"));


  const PORT = 8000;

  const server = http.createServer((req, res) => {
      if (req.method === 'POST' && req.headers['content-type'] === 'application/json') {
          let body = '';

          req.on('data', chunk => {
              body += chunk;
          });

          req.on('end', async () => {
              try {
                  parentPort.postMessage("tryGetTAda") 
                  const data = JSON.parse(body);
                  const pubKeyHashHex = data.pubKeyHashHex;

                  if (pubKeyHashHex) {
                      let ed25519KeyHash = csl.Ed25519KeyHash.from_hex(pubKeyHashHex);

                      let obj = {ed25519KeyHash : ed25519KeyHash, txHash : null};
                      getFundsFromFaucet(obj)();
                      await waitForTxHash(obj); 
                      res.writeHead(200, { 'Content-Type': 'application/json' });
                      res.end(JSON.stringify({ message: `Received pubKeyHashHex: ${pubKeyHashHex}; paid 1000 tada txHash : ${obj.txHash}`}));
                  } else {
                      res.writeHead(400, { 'Content-Type': 'application/json' });
                      res.end(JSON.stringify({ error: 'pubKeyHashHex field is required' }));
                  }
              } catch (err) {
                  res.writeHead(400, { 'Content-Type': 'application/json' });
                  res.end(JSON.stringify({ error: 'Invalid JSON' }));
              }
          });
      } else {
          res.writeHead(404, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Not Found' }));
      }
  });

  server.listen(PORT, () => {
      console.log(`Server is running on port ${PORT}`);
  });
};

faucet();

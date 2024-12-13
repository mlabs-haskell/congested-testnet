
function waitForTxHash(obj) {
  return new Promise((resolve) => {
    const interval = setInterval(() => {
      if (obj.txHash != null) {
        clearInterval(interval);
        const txHash = obj.txHash;
        obj.txHash = null;
        resolve(txHash);
      }
    }, 100); 
  });
}



const faucetServer = async () => {
  const http = await import("http");
  const {parentPort} = await import("node:worker_threads");
  const path = await import("node:path");
  const csl  = await import("@emurgo/cardano-serialization-lib-nodejs");
  const {faucet} = await import(path.resolve(__dirname, "../output/Spammer/index.js"));


  const PORT = 8000;

  console.log("create faucet server....")
  var faucetPars = {ed25519KeyHash : null, txHash : null, iWallet : 0, parentPort : parentPort, isAllowTransactions : false};
  faucet(faucetPars)();
  const server = http.createServer((req, res) => {
      if (req.method === 'POST' && req.headers['content-type'] === 'application/json') {
          let body = '';

          req.on('data', chunk => {
              body += chunk;
          });

          req.on('end', async () => {
              try {
                  const data = JSON.parse(body);
                  const pubKeyHashHex = data.pubKeyHashHex;

                  if (pubKeyHashHex) {
                      faucetPars.ed25519KeyHash = csl.Ed25519KeyHash.from_hex(pubKeyHashHex);
                      const txHash = await waitForTxHash(faucetPars); 
                      res.writeHead(200, { 'Content-Type': 'application/json' });
                      const message = `Received pubKeyHashHex: ${pubKeyHashHex};\n paid 1000 tada txHash : ${txHash}\n . Transaction added to mempool`;
                      res.end(JSON.stringify({ message }).replace(/\\n/g, '\n'));
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

faucetServer();

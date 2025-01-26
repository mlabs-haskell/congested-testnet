// MAIN 
// spammers and faucet share same wallets
// TODO  fix unlock , fix try in purs file, fix pause unpause , fix faucet
(async () => {
  const path = await import("path");

  //sate
  let state = await import(path.resolve(__dirname, "./state.js"));
  state = state.default;

  // run workers
  const { Worker } = await import("node:worker_threads");
  const workers = [];
  for (let i = 0; i < parseInt(process.env.N_WORKERS); i++) {
    workers.push(new Worker(path.resolve(__dirname, "./worker.js")));
  }
  handleExit(workers,state);

  // initialise worker wallets 
  while (state.walletsEmpty()) { 
    workers[0].postMessage(state.initializeWalletsPars());
    let txHash;
    await new Promise((resolve) => workers[0].once("message", msg => {txHash = state.handleMessage(msg); resolve()}));
    if (txHash) await awaitTxTime(txHash);
    await new Promise((resolve) => setTimeout(() => resolve(), 2000))
  }; 


  // message handler  
  let flagMeasureTxTimeInProcess = false;
  let txTimeSeconds;
  workers.map(w => w.on("message", msg => {
    const txHash = state.handleMessage(msg);
    // measure tx time for metrics 
    if (!flagMeasureTxTimeInProcess) 
     (async () => {
       flagMeasureTxTimeInProcess = true;
       txTimeSeconds = await awaitTxTime(txHash); 
       flagMeasureTxTimeInProcess = false;
     })(); 
    })); 

  // display tx time prometheus metric
  spawnMeasureTxTimePrometheusMetric(() => txTimeSeconds);

  // workers loop
  // (async () => {
  //   while (true) {
  //     workers.map(w => w.postMessage)
  //
  //   };
  //
  // })();


  // spammer contoller
  var spammerLoops;

  const runSpammers = () => {
    if (!spammerLoops) {
      // spammerLoops = workers.map(runSpammer(state)); 
      spammerLoops = workers.map(worker => setInterval(() => {
          msg = state.txPars();
          worker.postMessage(msg);
        },500)); 
      console.log("RESUME");
    }
  };

  const stopSpammers = () => {
    if (spammerLoops){
    console.log("PAUSE");
    spammerLoops.map(clearInterval); 
    spammerLoops = undefined;
    }
  };

  const {WebSocket} = await import("ws");
  const ws = new WebSocket(`ws://${process.env.OGMIOS_URL}:1337`);
  // handle mempool info
  ws.on("message", message => {
      let msg = JSON.parse(message);
      if (msg.method == "sizeOfMempool"){ 
        if (msg.result.currentSize.bytes > process.env.MEMPOOL_PAUSE_LIMIT) stopSpammers();
        else runSpammers();
      }}) 

  spawnMemPoolChecker(ws);
  spawnFaucet(workers, state);
})()



const spawnMemPoolChecker = async ws => {
  setInterval(() =>  
    [
      { method: "acquireMempool" },
      { method: "sizeOfMempool" },
      { method: "releaseMempool" },
    ].forEach((request) => {
      ws.send(
        JSON.stringify({
          jsonrpc: "2.0",
          method: request.method,
          params: {},
        })
      );
    }), 3000);
};

// request awaitTxTime with kupo
const awaitTxTime = async txHash => {
    console.log(`measure time for ${txHash}`)
    const fetch = await import("node-fetch");
    const url = `http://${process.env.KUPO_URL}:1442/matches/*@${txHash}`;
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

const spawnMeasureTxTimePrometheusMetric = async (timeInSecondsCallback) => {
    const { createServer } = await import("http");

    // Define the Prometheus exporter server
    const promExporter = createServer((req, res) => {
        if (req.url === '/metrics') {
            const metrics = `# TYPE await_time_tx gauge\nawait_time_tx ${timeInSecondsCallback()}\n`;
            res.writeHead(200, { 'Content-Type': 'text/plain' });
            res.end(metrics);
        } else {
            res.writeHead(404, { 'Content-Type': 'text/plain' });
            res.end('Not Found');
        }
    });

    // Start listening on the specified port
    const port = process.env.SPAMMER_METRIC_PORT; 
    promExporter.listen(port, () => {
      console.log(`Prometheus custom spammer metrics available at 0.0.0.0:${port}/metrics`);
    });
};


const handleExit = (workers, state) => {
  // handle exit
  ["SIGINT", "SIGTERM", "SIGQUIT"].forEach((signal) =>
    process.on(signal, () => {
      console.log("exit")
      state.saveState();
      workers.map(w => w.terminate())
      process.exit();
    })
  );

  process.on("uncaughtException", (error) => {
    console.error("Uncaught exception:", error);
    state.saveState();
    workers.map(w => w.terminate())
    process.exit(1);
  });

  process.on("unhandledRejection", (reason) => {
    console.error("Unhandled promise rejection:", reason);
    state.saveState();
    workers.map(w => w.terminate())
    process.exit(1);
  });
}

const spawnFaucet = async (workers, state) => {
  const http = await import("http");
  const FAUCET_PORT = process.env.FAUCET_PORT;
  console.log("create faucet server....")
  let i = 0;
  const server = http.createServer(
    (req, res) => {
      if (req.method === 'POST' && req.headers['content-type'] === 'application/json'){
        let body = '';
        req.on('data', chunk => {
            body += chunk;
        });
        req.on('end', async () => {
          try {
            const data = JSON.parse(body);
            const pubKeyHashHex = data.pubKeyHashHex;
            if (pubKeyHashHex) {
              workers[i].postMessage(state.faucetPayPars(pubKeyHashHex)); 

              i += 1;
              if (i == workers.length) i = 0;

              for (let i = 1; i <= 100; i++) {
                const txHash = state.faucetTxHash(pubKeyHashHex)
                if (txHash){
                  res.writeHead(200, { 'Content-Type': 'application/json' });
                  const message = {
                    txHash: txHash, 
                    msg : `${pubKeyHashHex} has paid with 1k tADA. Due to congestion, you neeed to wait until the transaction is added to block`
                  }
                  res.end(JSON.stringify({ message }));
                }; 
                await new Promise((resolve) => setTimeout(() => resolve(), 100))
            }}} catch (err) {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Invalid JSON' }));
          }})} else {
          res.writeHead(404, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ error: 'Not Found' }));
      }});

  server.listen(FAUCET_PORT, () => {
      console.log(`faucet server is running on port ${FAUCET_PORT}`);
  });
};
                            
// msg : `${pubKeyHashHex} has paid with 1k tADA. Due to congestion, you neeed to wait until the transaction is added to block`

// MAIN
// spammers and faucet share same wallets

main().catch(console.error);

async function main() {
  const path = await import("path");

  //state
  let state = await import(path.resolve(__dirname, "./state.js"));
  state = state.default;

  // run workers
  const { Worker } = await import("node:worker_threads");
  const workers = [];
  for (let i = 0; i < parseInt(process.env.N_WORKERS); i++) {
    workers.push(new Worker(path.resolve(__dirname, "./worker.js")));
  }
  handleExit(workers, state);

  // initialise worker wallets
  while (state.walletsEmpty()) {
    workers[0].postMessage(state.initializeWalletsPars());
    let txHash = await receiveWorkerMessage(workers[0], state);
    if (txHash) {
      const time = await awaitTxTime(txHash);
      console.log(`tx added to block in time ${time}`);
    }
    await sleep(2000);
  }

  // message handler
  let flagMeasureTxTimeInProcess = false;
  let txTimeSeconds = 0.0;
  workers.forEach(worker => {
    worker.on("message", async (msg) => {
      const txHash = state.handleMessage(msg);
      // measure tx time for metrics
      if (!flagMeasureTxTimeInProcess) {
        flagMeasureTxTimeInProcess = true;
        txTimeSeconds = await awaitTxTime(txHash);
        flagMeasureTxTimeInProcess = false;
      }
    });
  });

  // display tx time prometheus metric
  spawnMeasureTxTimePrometheusMetric(() => txTimeSeconds);

  // spammer controller
  var spammerLoops;

  const runSpammers = () => {
    if (!spammerLoops) {
      spammerLoops = workers.map(worker =>
        setInterval(() => {
          const msg = state.txPars();
          worker.postMessage(msg);
        }, 500),
      );
      console.log("RESUME");
    }
  };

  const stopSpammers = () => {
    if (spammerLoops) {
      console.log("PAUSE");
      spammerLoops.forEach(clearInterval);
      spammerLoops = undefined;
    }
  };

  const { WebSocket } = await import("ws");
  const ws = new WebSocket(`ws://${process.env.OGMIOS_URL}:1337`);

  // handle mempool info . 
  // If mempool size is too big, stop spammers, or resume if mempool size is too small
  ws.on("message", (message) => {
    const msg = JSON.parse(message);
    if (msg.method == "sizeOfMempool") {
      if (msg.result.currentSize.bytes > process.env.MEMPOOL_PAUSE_LIMIT)
        stopSpammers();
      else runSpammers();
    }
  });
  
  if (process.env.SPAMMER_ON == "true") {
    spawnMemPoolChecker(ws);
  }

  if (process.env.FAUCET_ON == "true") {
    spawnFaucet(workers, state);
  }
}

// Helper function to receive a message from a worker
async function receiveWorkerMessage(worker, state) {
  return new Promise(resolve => {
    worker.once("message", msg => {
      const txHash = state.handleMessage(msg);
      resolve(txHash);
    });
  });
}

// Helper function for sleep/delay
async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

const spawnMemPoolChecker = async ws => {
  console.log("start mempool checker and spammer");
  
  function sendMempoolRequest(request) {
    ws.send(
      JSON.stringify({
        jsonrpc: "2.0",
        method: request.method,
        params: {},
      })
    );
  }
  
  setInterval(() => {
    [
      { method: "acquireMempool" },
      { method: "sizeOfMempool" },
      { method: "releaseMempool" },
    ].forEach(sendMempoolRequest);
  }, 3000);
};

// request awaitTxTime with kupo
const awaitTxTime = async txHash => {
  console.log(`measure time for ${txHash}`);
  const fetch = await import("node-fetch");
  const url = `http://${process.env.KUPO_URL}:1442/matches/*@${txHash}`;
  const start = Date.now();
  
  while (true) {
    try {
      const resp = await fetch.default(url);
      const body = await resp.json();
      if (body.length > 0) return (Date.now() - start) / 1000;
    } catch (err) {
      throw Error("kupo requests error");
    }
    await sleep(4000);
  }
};

const spawnMeasureTxTimePrometheusMetric = async timeInSecondsCallback => {
  const { createServer } = await import("http");

  // Define the Prometheus exporter server
  function handleMetricsRequest(req, res) {
    if (req.url === "/metrics") {
      const metrics = `# TYPE await_time_tx gauge\nawait_time_tx ${timeInSecondsCallback()}\n`;
      res.writeHead(200, { "Content-Type": "text/plain" });
      res.end(metrics);
    } else {
      res.writeHead(404, { "Content-Type": "text/plain" });
      res.end("Not Found");
    }
  }
  
  const promExporter = createServer(handleMetricsRequest);

  // Start listening on the specified port
  const port = process.env.SPAMMER_METRIC_PORT;
  
  function onServerStart() {
    console.log(
      `Prometheus custom spammer metrics available at 0.0.0.0:${port}/metrics`
    );
  }
  
  promExporter.listen(port, onServerStart);
};

const handleExit = (workers, state) => {
  // handle exit
  function exitHandler() {
    console.log("exit");
    state.saveState();
    workers.forEach(w => w.terminate());
    process.exit();
  }
  
  function errorHandler(message, error) {
    console.error(message, error);
    state.saveState();
    workers.forEach(w => w.terminate());
    process.exit(1);
  }
  
  ["SIGINT", "SIGTERM", "SIGQUIT"].forEach(signal => {
    process.on(signal, exitHandler);
  });

  process.on("uncaughtException", error => {
    errorHandler("Uncaught exception:", error);
  });

  process.on("unhandledRejection", reason => {
    errorHandler("Unhandled promise rejection:", reason);
  });
};

const spawnFaucet = async (workers, state) => {
  const path = await import("path");
  const http = await import("http");
  const { keyHashFromHex } = await import(
    path.resolve(__dirname, "./utils.js")
  );

  const FAUCET_PORT = process.env.FAUCET_PORT;
  console.log("create faucet server....");
  let i = 0;
  
  async function processRequest(body, res) {
    try {
      const data = JSON.parse(body);
      const pubKeyHashHex = data.pubKeyHashHex;
      // trigger exception if pubKeyHashHex is wrong
      _ = keyHashFromHex(pubKeyHashHex);

      if (pubKeyHashHex) {
        workers[i].postMessage(state.faucetPayPars(pubKeyHashHex));
        i += 1;
        if (i == workers.length) i = 0;

        for (let i = 1; i <= 100; i++) {
          const txHash = state.faucetTxHash(pubKeyHashHex);
          if (txHash) {
            res.writeHead(200, { "Content-Type": "application/json" });
            const message = {
              txHash: txHash,
              msg: `${pubKeyHashHex} has paid with 1k tADA. Due to congestion, you neeed to wait until the transaction is added to block`,
            };
            res.end(JSON.stringify({ message }));
            return;
          }
          await sleep(100);
        }
      }
    } catch (err) {
      res.writeHead(400, { "Content-Type": "application/json" });
      res.end(
        JSON.stringify({ error: "Invalid JSON, or wrong pubKeyHashHex" }),
      );
    }
  }
  
  function handleRequest(req, res) {
    if (
      req.method === "POST" &&
      req.headers["content-type"] === "application/json"
    ) {
      let body = "";
      
      function collectData(chunk) {
        body += chunk;
      }
      
      function processData() {
        processRequest(body, res);
      }
      
      req.on("data", collectData);
      req.on("end", processData);
    } else {
      res.writeHead(404, { "Content-Type": "application/json" });
      res.end(JSON.stringify({ error: "Not Found" }));
    }
  }
  
  const server = http.createServer(handleRequest);

  function onServerStart() {
    console.log(`faucet server is running on port ${FAUCET_PORT}`);
  }
  
  server.listen(FAUCET_PORT, onServerStart);
};

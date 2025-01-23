// MAIN 
// spammers and faucet share same wallets
(async () => {
  const path = await import("path");
  let state = await import(path.resolve(__dirname, "./state.js"));
  state = state.default;
  const { Worker } = await import("node:worker_threads");

  // run workers
  const workers = [];
  for (let i = 0; i < parseInt(process.env.N_WORKERS); i++) {
    workers.push(new Worker(path.resolve(__dirname, "./worker.js")));
  }

  workers.map(w => w.on("message", msg => state.handleMessage(msg)));

  // setup event manager
  // const event = await import("events");
  // const EVENT = new event.EventEmitter();
  // let spammerLoops;
  // EVENT.on("unpauseSpammer", () => {spammerLoops = workers.map(w => runSpammer(w,state))});
  // EVENT.on("pauseSpammer", () => {spammerLoops.map(loop => clearInterval(loop))});
  


  // initialise worker wallets 
  if (state.walletsEmpty()) {
    workers[0].postMessage(state.initializeWalletsPars());
    // await workerResponse(workers[0],state);
  };

  // run spammer loops
  loops = workers.map(runSpammerLoop(state));


  // workers[0].on("message", msg => {console.log(msg)})
  // await workerResponse(workers[0],state);
  // workers[0].postMessage(state.txPars());
  // await workerResponse(workers[0],state);
  // workers[0].postMessage(state.txPars());
  // await workerResponse(workers[0],state);
  // workers[0].postMessage(state.txPars());
  // const loop = runSpammer(workers[0], state);
  // console.log(loop);
  // setTimeout(() => {loops.map(clearInterval); console.log("clear");}, 2000);
  // EVENT.emit("unpauseSpammer");

  // pause if large mempool
  // spawnMemPoolChecker(state.default, EVENT);
  // // await tx metrics
  // spawnAwaitTxMetric(state.default, EVENT);
  // // faucet based on workers
  // spawnFaucet(state.default);
  // spammerLoops = workers.map(w => runSpammer(w,state))
})()

// const runSpammer = async (worker, state) => {
//
//   const loop = setInterval(async () => {
//       worker.postMessage(state.txPars());
//       await workerResponse(worker, state);
//   },10);
//   return loop;
// };

// const workerResponse =  (worker, state) => (
//   new Promise((resolve) => {
//     worker.once("message", msg => {resolve(state.handleMessage(msg));})
//   })
// );

const runSpammerLoop = state => worker => (setInterval(() => {
    worker.postMessage(state.txPars());
    },10));

// const runSpammer = (workers, state) => {
//   // const loop = w => (setInterval(() => console.log(state.txPars()),100));
//   const loop = w => (setInterval(() => {},100));
//   return workers.map(loop)
// };

// const runSpammer = async (state, workers) => {
//   // TODO handle response from worker
//   const loop = setInterval(async () => {
//       const worker = workers[i];
//       worker.postMessage(state.txPars());
//       i += 1;
//       if (i == workers.length) i = 0;
//   },0);
//   return loop;
// };


// const spawnWorker = async (state) => {
//   const path = await import("path");
//   const { Worker } = await import("node:worker_threads");
//   const worker = new Worker(path.resolve(__dirname, "./worker.js"));
//   worker.on("message", (msg) => {
//     state.updateState(msg);
//     worker.postMessage(state.message());
//   });
// };
//


const spawnMemPoolChecker = async (state) => {
  const {WebSocket} = await import("ws");
  const ws = new WebSocket(`ws://${process.env.OGMIOS_URL}:1337`);
  setInterval(() => sendMempoolRequests(ws), 3000);
  ws.on("message", state.updateMemPoolSize);
};

//
//
// const spawnAwaitTxMetric = async (state) => {
//     // Dynamically import the HTTP module
//     const { createServer } = await import("http");
//
//     // Define the Prometheus exporter server
//     const promExporter = createServer((req, res) => {
//         if (req.url === '/metrics') {
//             const metrics = `# TYPE await_time_tx gauge\nawait_time_tx ${state.awaitTxTime()}\n`;
//             res.writeHead(200, { 'Content-Type': 'text/plain' });
//             res.end(metrics);
//         } else {
//             res.writeHead(404, { 'Content-Type': 'text/plain' });
//             res.end('Not Found');
//         }
//     });
//
//     // Start listening on the specified port
//     const port = process.env.SPAMMER_METRIC_PORT; 
//     promExporter.listen(port, () => {
//       console.log(`Prometheus custom spammer metrics available at 0.0.0.0:${port}/metrics`);
//     });
// };
//
// // send mempoort request to ogmios
// const sendMempoolRequests = (ws) => {
//   const requests = [
//     { method: "acquireMempool" },
//     { method: "sizeOfMempool" },
//     { method: "releaseMempool" },
//   ];
//
//   requests.forEach((request) => {
//     ws.send(
//       JSON.stringify({
//         jsonrpc: "2.0",
//         method: request.method,
//         params: {},
//       })
//     );
//   });
// };
//
// const handleWebSocketMessage = (state) => (data) => {
//   try {
//     const resp = JSON.parse(data);
//
//     if (resp.method === "sizeOfMempool") {
//       const memPoolSize = resp.result.currentSize.bytes;
//
//       if (memPoolSize > process.env.MEMPOOL_PAUSE_LIMIT) {
//         console.log("PAUSE");
//         state.pause();
//       } else if (memPoolSize < process.env.MEMPOOL_UNPAUSE_LIMIT) {
//         console.log("UNPAUSE");
//         state.unpause();
//       }
//     }
//   } catch (error) {
//     console.error("Error handling WebSocket message:", error);
//   }
// };
//
//
//
//
//
// const spawnFaucet = async (state) => {
//   const http = await import("http");
//   const FAUCET_PORT = process.env.FAUCET_PORT;
//   console.log("create faucet server....")
//   const server = http.createServer((req, res) => {
//       if (req.method === 'POST' && req.headers['content-type'] === 'application/json') {
//           let body = '';
//           req.on('data', chunk => {
//               body += chunk;
//           });
//           req.on('end', async () => {
//               try {
//                   const data = JSON.parse(body);
//                   const pubKeyHashHex = data.pubKeyHashHex;
//                   state.faucetPay(pubKeyHashHex);
//                   if (pubKeyHashHex) {
//                       const interval = setInterval(() => {
//                         let txHash = state.faucetFinish(pubKeyHashHex)
//                         if (txHash) {
//                           clearInterval(interval);
//                           clearTimeout(timeout);
//                           res.writeHead(200, { 'Content-Type': 'application/json' });
//                           const message = {
//                             txHash: txHash, 
//                             msg : `${pubKeyHashHex} has paid with 1k tADA. Due to congestion, you neeed to wait until the transaction is added to block`
//                           }
//                           // res.end(JSON.stringify({ message }).replace(/\\n/g, '\n'));
//                           res.end(JSON.stringify({ message }));
//                         };
//                       },100);
//                       const timeout = setTimeout(() => {
//                         clearInterval(interval); // Stop the interval
//                         res.write('Error: Timeout! backend error.\n');
//                         res.end(); // End the response with an error message
//                       }, 150000);
//                   } else {
//                       res.writeHead(400, { 'Content-Type': 'application/json' });
//                       res.end(JSON.stringify({ error: 'pubKeyHashHex field is required' }));
//                   }
//               } catch (err) {
//                   res.writeHead(400, { 'Content-Type': 'application/json' });
//                   res.end(JSON.stringify({ error: 'Invalid JSON' }));
//               }
//           });
//       } else {
//           res.writeHead(404, { 'Content-Type': 'application/json' });
//           res.end(JSON.stringify({ error: 'Not Found' }));
//       }
//   });
//
//   server.listen(FAUCET_PORT, () => {
//       console.log(`faucet server is running on port ${FAUCET_PORT}`);
//   });
// };
//
//
// const handleWorkerMessage = (msg, worker, state) => {
//   // request messages from worker
//   if (msg === "backendPars") {
//     worker.postMessage(state.backendPars());
//   } else if (msg === "txPars") {
//     worker.postMessage(state.txPars());
//   // tx results messages from worker
//   } else  {
//     state.processTxResult(msg);
//     worker.postMessage("ok");
//   }
// };
//
// const isDetailedMessage = (msg) => {
//   return Object.hasOwn(msg, "msg") && Object.hasOwn(msg, "time");
// };
//
// const processDetailedMessage = (msg, state, worker) => {
//   const { msg: message, time } = msg;
//
//   if (message.startsWith("initializedWallets")) {
//     state.unpause();
//     state.setWalletsInitiated();
//     state.noAwaitTxToMeasureTime();
//   } else if (message.startsWith("locked")) {
//     const [_, lockedScript, lockedTxHash] = message.split("_");
//     state.pushLocked(lockedScript, lockedTxHash);
//   } else if (message.startsWith("unlocked")) {
//     const [_, txHash] = message.split("_");
//     state.clearLocked(txHash);
//   } else if (message.startsWith("paid")) {
//     const [_, pKeyHash, txHash] = message.split("_");
//     // mark if faucet paid 
//     state.faucetPaid(pKeyHash, txHash);
//   }
//
//   if (time) {
//     console.log(`time : ${time}`);
//     state.setAwaitTxTime(time);
//     state.noAwaitTxToMeasureTime();
//   }
//
//   worker.postMessage("ok");
// };
//

const spawnMemPoolChecker = async (state) => { 
  const {WebSocket} = await import("ws");
  const ws =  new WebSocket(`ws://${process.env.OGMIOS_URL}:${process.env.OGMIOS_PORT}`);
  ws.on('message', (data) => {
      const resp = JSON.parse(data);
      if (resp.method == 'sizeOfMempool') {
        let memPoolSize = resp.result.currentSize.bytes;
        if (memPoolSize > process.env.MEMPOOL_PAUSE_LIMIT) {
          console.log("PAUSE")
          state.pause();
        } else if (memPoolSize < process.env.MEMPOOL_UNPAUSE_LIMIT) {
          console.log("UNPAUSE")
          state.unpause();
        }
      };
    }
  )
  setInterval(() => {
      ws.send(
        JSON.stringify({
          jsonrpc: '2.0',
          method : 'acquireMempool',
          params : {}
        })
      );
      ws.send(
        JSON.stringify({
          jsonrpc: '2.0',
          method : 'sizeOfMempool',
          params : {}
        })
      );
      ws.send(
        JSON.stringify({
          jsonrpc: '2.0',
          method : 'releaseMempool',
          params : {}
        })
      );
    },3000);
};


const spawnAwaitTxMetric = async (state) => {
    const http = await import("http");
    const promExporter = http.createServer((req, res) => {
        if (req.url === '/metrics') {
            res.writeHead(200, { 'Content-Type': 'text/plain' });
            res.end(`# TYPE await_time_tx gauge\nawait_time_tx ${state.awaitTxTime }\n`);
        } else {
            res.writeHead(404, { 'Content-Type': 'text/plain' });
            res.end('Not Found');
        }
    });

     promExporter.listen(process.env.SPAMMER_METRIC_PORT, () => {
         console.log(`Prometheus metrics available at http://0.0.0.0:${process.env.SPAMMER_METRIC_PORT}/metrics`);
     });

};


const spawnWorker = async (state) => {
   const path = await import("path");
   const {Worker} = await import("node:worker_threads");
   const worker = new Worker(path.resolve(__dirname, "./worker.js"));
   worker.on("message", msg => {
     if (msg == "backendPars") {
       worker.postMessage(state.backendPars())
     } else if (msg == "txPars") {
       worker.postMessage(state.txPars());
     } else if (Object.hasOwn(msg,"msg") && Object.hasOwn(msg,"time")) {
       let message = msg.msg;
       let time = msg.time;
       if (message.startsWith("initializedWallets")) {
         state.setUnPause();
         state.setWalletsInitiated(); 
         state.setAwaitTxTimeNotInProcess(); 
       };
       if (message.startsWith("locked")) {
         let [_, lockedScript, lockedTxHash ] = message.split("_"); 
         state.pushLocked(lockedScript, lockedTxHash);
       };
       if (message.startsWith("unlocked")) {
         let [_, txHash] = message.split("_"); 
         state.clearLocked(txHash);
       }; 
       if (time) {
         // reset
         console.log(`time : ${time}`)
         state.noAwaitTxToMeasureTime(); 
       };
       worker.postMessage("ok");
     } else {
       worker.postMessage("ok");
     };
   });

   worker.on("error", (error) => {
     console.error("Worker encountered an error:", error);
   });

   // Handle the exit of the worker and restart if needed
   worker.on("exit", (code) => {
     if (code !== 0) {
       console.error(`Worker exited with code ${code}. Restarting...`);
       spawnWorker(state); 
     } else {
       console.log("Worker exited gracefully.");
     }
   });
};

// MAIN 
(async () => {
  // generate wallets and fill them with funds 
  // spammers and faucet share same wallets
  const path = await import("path");
  const state = await import(path.resolve(__dirname, "./state.js"));
  for (let i = 0; i < parseInt(process.env.N_WORKERS); i++) {
     spawnWorker(state);
  }
  // pause if large mempool
  spawnMemPoolChecker(state);

  
  // // console.log(`ws://${process.env.OGMIOS_URL}:${process.env.OGMIOS_PORT}`)
  // const workers = Array.from({length : nSpammers},(_,spammerId) => {
  //    let wData = {spammerId : spammerId, waitTx : (spammerId == nSpammers - 1) ? true : false };
  //    return new Worker(path.resolve(__dirname, "./spammer.js"), {workerData: wData})
  // }
  // ) 
  //   // ogmios websocket connection
  //   const {WebSocket} = await import("ws");
  // 
  //   const ws =  new WebSocket(`ws://${process.env.OGMIOS_URL}:${process.env.OGMIOS_PORT}`);
  //   // stop spammers if mempool is large and proceed, if mempool is small
  //   ws.on('message', (data) => {
  //     const resp = JSON.parse(data);
  //     if (resp.method == 'sizeOfMempool') {
  //       let memPoolSize = resp.result.currentSize.bytes;
  //       // pause / unpause
  //       if (memPoolSize > process.env.MEMPOOL_UP_LIMIT) {
  //          spammers.map(sp => sp.postMessage("pause"));
  //       } else if (memPoolSize < process.env.MEMPOOL_LO_LIMIT) {
  //          spammers.map(sp => sp.postMessage("unpause"));
  //       }
  //     }
  //   });
  //
  // // spammers
  // // activate wallets in each spammer one by one 
  // for (let i = 0; i < nSpammers - 1; i++) {
  //     spammers[i].on(
  //      "message",
  //       msg => {
  //        if (msg == "successfullyPaidToSpammerWallet") {
  //            spammers[i+1].postMessage("unpause")
  //         };
  //        }); 
  //     };
  //   spammers[nSpammers - 1].on(
  //      "message", msg => { 
  //        if (msg == "successfullyPaidToSpammerWallet")   {
  //            console.log("last spammer initialized. unpause all spammers")
  //            spammers.map(sp => sp.postMessage("unpause"));
  //            startMempoolChecker(ws);
  //         };
  //      }
  //   )
  //   spammers.map(sp => sp.on("error", async error => { console.error(error); process.exit(1);}));
  //   spammers.map(sp => sp.on("exit", async error => { console.error(error); process.exit(1);}));
  //   // spammers[0].postMessage("unpause");
  // 
  //   // faucet 
  //   const faucet = new Worker(path.resolve(__dirname, "./faucet.js"), {workerData: null}) 
  //   // pause spammers to get tada
  //   faucet.on("message", async msg => {
  //    if (msg == "successfullyPaidToSpammerWallet")   {
  //        console.log("fill 1st spammer")
  //        spammers[0].postMessage("unpause");
  //     };
  //   })
  //   faucet.on("error", async error => {
  //     console.error(error);
  //     process.exit(1);
  //   })
  //
  //   faucet.on("exit", async code => {
  //     console.error(code);
  //     process.exit(1);
  //   })
})()




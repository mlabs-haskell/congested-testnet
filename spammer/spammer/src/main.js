// const { type } = require("node:os");
//
// const pauseSpammersnSec = spammers => nsec => new Promise((resolve) => {
//     spammers.map(sp => sp.postMessage("pause"));
//     let i = 0;
//     const interval = setInterval(() => {
//       i += 1;
//       if (i > nsec) {
//         clearInterval(interval);
//         spammers.map(sp => sp.postMessage("unpause"));
//         resolve();
//       }
//     }, 1000); 
//   });
//
// const startMempoolChecker = async ws =>  setInterval(() => {
//       ws.send(
//         JSON.stringify({
//           jsonrpc: '2.0',
//           method : 'acquireMempool',
//           params : {}
//         })
//       );
//       ws.send(
//         JSON.stringify({
//           jsonrpc: '2.0',
//           method : 'sizeOfMempool',
//           params : {}
//         })
//       );
//       ws.send(
//         JSON.stringify({
//           jsonrpc: '2.0',
//           method : 'releaseMempool',
//           params : {}
//         })
//       );
//     },4000);

// const initParams = async () => {
//   pars = {
//     N_WORKERS : parseInt(process.env.N_WORKERS)
//   };
//   return pars;
// };

// const createWorkerWallets = async () => {
// };
//
const txResponseFromState = (state) => {
  resp = {
    type : "TxPars",
    amount : "1000000000000",
    await : null,
    tx : "skip",
    from : null,
    to : [],
    script : null,
    utxo : null,
  };

  if (state.mainWalletFree() && state.walletsEmpty()) {
    // initialize spammer wallets
    state.setMainWalletBusy()
    resp.tx = "initWallets";
    resp.to = state.walletHashes();
  } else if (state.pause()) {
    resp.tx = "skip";
  } else {
    resp.tx = "pay";
    resp.from = state.wallets.keys[];
    resp.to = state.wallets.hashes;
  };
  // console.log(`resp is : ${resp}`);
  return resp;
};


const spawnWorker = async (state) => {
   const path = await import("path");
   const {Worker} = await import("node:worker_threads");
   const worker = new Worker(path.resolve(__dirname, "./worker.js"));
   worker.on("message", msg => {
     if (msg == "reqBackendPars") {
       worker.postMessage({
         type : "BackendPars",
         ogmiosUrl : state.ogmiosUrl,
         kupoUrl : state.kupoUrl,
         walletPath : state.mainWallet.path})
     } else if (msg == "reqNextTransaction") {
       worker.postMessage(txResponseFromState(state));
     } else if (msg == "walletsFilled") {
       state.mainWallet.isInUse = false;
       state.wallets.isFilled = true;
       worker.postMessage({ type : "OK" })
     };
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




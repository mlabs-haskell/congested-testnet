const pauseSpammersnSec = spammers => nsec => new Promise((resolve) => {
    spammers.map(sp => sp.postMessage("pause"));
    let i = 0;
    const interval = setInterval(() => {
      i += 1;
      if (i > nsec) {
        clearInterval(interval);
        spammers.map(sp => sp.postMessage("unpause"));
        resolve();
      }
    }, 1000); 
  });

const startMempoolChecker = async ws =>  setInterval(() => {
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
    },4000);

(async () => {
  const {Worker, workerData} = await import("node:worker_threads");
  const path = await import("node:path");
  const nSpammers = 3 
  const spammers = Array.from({length : nSpammers},(_,spammerId) => {
     let wData = {spammerId : spammerId, waitTx : (spammerId == nSpammers - 1) ? true : false };
     return new Worker(path.resolve(__dirname, "./spammer.js"), {workerData: wData})
  }
  ) 
    // ogmios websocket connection
    const {WebSocket} = await import("ws");
    const ws =  new WebSocket("ws://0.0.0.0:1337");
    // stop spammers if mempool is large and proceed, if mempool is small
    ws.on('message', (data) => {
      const resp = JSON.parse(data);
      if (resp.method == 'sizeOfMempool') {
        let memPoolSize = resp.result.currentSize.bytes;
        // pause / unpause
        if (memPoolSize > 100000 ) {
           spammers.map(sp => sp.postMessage("pause"));
        } else if (memPoolSize < 60000 ) {
           spammers.map(sp => sp.postMessage("unpause"));
        }
      }
    });

  // spammers
  // activate wallets in each spammer one by one 
  for (let i = 0; i < nSpammers - 1; i++) {
      spammers[i].on(
       "message",
        msg => {
         if (msg == "successfullyPaidToSpammerWallet") {
             spammers[i+1].postMessage("unpause")
          };
         }); 
      };
    spammers[nSpammers - 1].on(
       "message", msg => { 
         if (msg == "successfullyPaidToSpammerWallet")   {
             console.log("last spammer initialized. unpause all spammers")
             spammers.map(sp => sp.postMessage("unpause"));
             startMempoolChecker(ws);
          };
       }
    )
    spammers.map(sp => sp.on("error", async error => { console.error(error); process.exit(1);}));
    spammers.map(sp => sp.on("exit", async error => { console.error(error); process.exit(1);}));
  
    // faucet 
    const faucet = new Worker(path.resolve(__dirname, "./faucet.js"), {workerData: null}) 
    // pause spammers to get tada
    faucet.on("message", async msg => {
     if (msg == "successfullyPaidToSpammerWallet")   {
         console.log("fill 1st spammer")
         spammers[0].postMessage("unpause");
      };
    })
    faucet.on("error", async error => {
      console.error(error);
      process.exit(1);
    })

    faucet.on("exit", async code => {
      console.error(code);
      process.exit(1);
    })



})()




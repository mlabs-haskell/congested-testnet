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

(async () => {
  const {Worker, workerData} = await import("node:worker_threads");
  const path = await import("node:path");
  const nSpammers = 2 
  const spammers = Array.from({length : nSpammers},(_,idSpammer) => new Worker(path.resolve(__dirname, "./spammer.js"), {workerData: idSpammer})) 
  // spammers
  // activate wallets in each spammer one by one 
  for (let i = 0; i < nSpammers; i++) {
      spammers[i].on(
        "message",
        msg => {
         if (msg == "successfullyPaidToSpammerWallet") {
           if (i < nSpammers - 1){
             spammers[i+1].postMessage("unpause")
          } else {
            // if last is activated start loops in all spammers
             console.log("unpause all")
             spammers.map(sp => sp.postMessage("unpause"));
          };
         }; 
        }
      ) 
  }

  // start measure tx time 
  const measureTxTime = new Worker(path.resolve(__dirname, "./measureTxTime.js"), {workerData: null}) 
  measureTxTime.on("message", msg => {
      if (msg == "successfullyPaidToSpammerWallet") {
        spammers[0].postMessage("unpause") 
      }
  })


  // faucet 
  const faucet = new Worker(path.resolve(__dirname, "./faucet.js"), {workerData: null}) 
  faucet.on("message", async msg => {
    // pause spammers to get tada
    if (msg == "tryGetTAda") {
       await pauseSpammersnSec(spammers)(8)
    }
  })


})()




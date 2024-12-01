(async () => {
  const {Worker, workerData} = await import("node:worker_threads");
  const path = await import("node:path");
  const nSpammers = 2 
  // const spammers = Array.from({length : nSpammers},(_,idSpammer) => new Worker(path.resolve(__dirname, "./spammer.js"), {workerData: idSpammer})) 
  // // activate wallets in each spammer one by one 
  // for (let i = 0; i < nSpammers; i++) {
  //     spammers[i].on(
  //       "message",
  //       msg => {
  //        if (msg == "successfullyPaidToSpammerWallet") {
  //          if (i < nSpammers - 1){
  //            spammers[i+1].postMessage("unpause")
  //         } else {
  //           // if last is activated start loops in all spammers
  //            spammers.map(sp => sp.postMessage("unpause"));
  //         };
  //        }; 
  //       }
  //     ) 
  // }
  // spammers[0].postMessage("unpause") 
  // start faucet 
  const faucet = new Worker(path.resolve(__dirname, "./faucet.js"), {workerData: null}) 


})()




(async () => {
    const {Worker, workerData} = await import("node:worker_threads");
  const nSpammers = 2
  var iSpammerAllowTransitions = 0
  // run spammers
  for (let iSpammer = 0; iSpammer < nSpammers; iSpammer++ ){
    const spammer = new Worker("./src/spammer.js", {workerData: iSpammer});
    spammer.postMessage(iSpammerAllowTransitions)
    spammer.on('message', (msg) => {
         console.log("GET MESSAGE!!!!!!!!!!!!!!!!!!!")
         if (msg == "paidToWallets") {
           iSpammerAllowTransitions++
         }
        });
  }
})()




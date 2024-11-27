(async () => {
    const {workerData, parentPort} = await import("node:worker_threads");
    const {spammer} = await import("../output/Spammer/index.js");
    const {_new, write, read} = await import("../output/Effect.Ref/foreign.js");
    let spammerId = workerData
    
    let controlVars = _new({
      paidToWallets : false, 
      allowTransactions: false
    })();
  


    parentPort.on("message", spammerIdAllowTransactions => { 
      if (spammerIdAllowTransactions  == spammerId) {
        console.log(`allow tx ${spammerId}`)
        console.log(controlVars)
        let controlVars_ = read(controlVars)()
        write({
          paidToWallets : controlVars_.paidToWallets, 
          allowTransactions: true,
        })(controlVars)();
        console.log(controlVars)
      }
    });

    spammer(parentPort)(spammerId)(controlVars)();
    
})()



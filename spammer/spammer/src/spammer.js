worker =  async () => {
    const {workerData, parentPort} = await import("node:worker_threads");
    const {spammer} = await import("../output/Spammer/index.js");
    // const {_new, write, read} = await import("../output/Effect.Ref/foreign.js");
    const stopSpammerCallback = async () => {
      parentPort.postMessage("stopSpammer")
    }
    
    const paidToWalletsCallback = async () => {
      parentPort.postMessage("paidToWallets")
    }

    controlVars = _new({paidToWallets : false, 
      allowedToSubmitTransactions: false,
      allowedToSubmitTransactions: false,
      stopSpammerCallback : stopSpammerCallback,
      paidToWalletsCallback : paidToWalletsCallback 
    })();
  

    // setInterval(() => {
    //  _s = read(s)();
    //  if (_s.paidToWallets){
    //    console.log("paid------------------------------------------------")
    //  }
    // }, 10)

    // run spammer loop
    // spammer(controlVars)()

    parentPort.on("message", msg = {
      if (msg == "stop") {

      }
    })
    
}


worker()

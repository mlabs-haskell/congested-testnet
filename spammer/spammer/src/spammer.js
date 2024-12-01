(async () => {
    const {workerData, parentPort} = await import("node:worker_threads");
    const path = await import("path");
    const {spammer} = await import(path.resolve(__dirname, "../output/Spammer/index.js"));
    
    var controlVars = {
        idSpammer : workerData, 
        isAllowTransactions: false,
        parentPort : parentPort
      }
  

    parentPort.on("message", msg => { 
      if (msg == "pause") {
        console.log(`pause spammerID : ${workerData}`)
        controlVars.isAllowTransactions = false;
      };
      if (msg == "unpause") {
        console.log(`unpause spammerID : ${workerData}`)
        controlVars.isAllowTransactions = true;
      };
    });

    spammer(controlVars)();
})()



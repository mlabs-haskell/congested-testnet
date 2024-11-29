(async () => {
    const {workerData, parentPort} = await import("node:worker_threads");
    const {spammer} = await import("../output/Spammer/index.js");
    
    let controlVars = {
        idSpammer : workerData, 
        isAllowTransactions: false,
        parentPort : parentPort
      }
  

    parentPort.on("message", msg => { 
      console.log(msg);
      if (msg == "pause") {
        controlVars.isAllowTransactions = false;
      };
      if (msg == "unpause") {
        controlVars.isAllowTransactions = true;
      };
    });

    spammer(controlVars)();
})()



(async () => {
   const path = await import("path");
   const {parentPort} = await import("node:worker_threads");
   const {executeTransactionLoop} = await import(path.resolve(__dirname, "../output/Utils/index.js"));
   var state = {};
    
   parentPort.on("message", resp => {
         Object.assign(state, resp);
   });

   executeTransactionLoop(parentPort)(state)();
})()


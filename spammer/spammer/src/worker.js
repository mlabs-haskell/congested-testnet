(async () => {
   const path = await import("path");
   const {parentPort} = await import("node:worker_threads");
   // const EventEmitter = await import("node:events");
   const {executeTransactionLoop} = await import(path.resolve(__dirname, "../output/Utils/index.js"));
   // const eventEmitter = new EventEmitter();
   // var state = {};

   // copy response to state, in order interact with purescript command
   // parentPort.on("message", resp => {
   //       Object.assign(state, resp);
   // });

   // eventEmitter.on("requestTx", () => {
   //    parentPort.postMessage("requestTx");
   // });
   //
   // eventEmitter.on("clearState", () => {
   //     Object.keys(state).forEach(key => {
   //        delete state[key];
   //      });
   // });

    
   // executeTransactionLoop(eventEmitter)(state)();
   executeTransactionLoop(parentPort)();
})()


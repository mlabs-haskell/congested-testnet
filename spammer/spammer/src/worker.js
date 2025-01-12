(async () => {
   const path = await import("path");
   const {parentPort} = await import("node:worker_threads");
   const {executeTransactionLoop} = await import(path.resolve(__dirname, "../output/Worker/index.js"));
   executeTransactionLoop(parentPort)();
})()


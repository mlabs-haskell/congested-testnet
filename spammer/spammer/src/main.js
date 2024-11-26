(async () => {
    const {Worker } = await import("node:worker_threads");
    const {spammer} = await import("../output/Spammer/index.js");
    // new Worker("./src/workers.js", {workerData: "hello"});
    console.log("hello")
})()




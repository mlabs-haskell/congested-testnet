const main =  async () => {
    const {spammer} = await import("../spammer/output/SpammerUtils/index.js");
    const {Worker } = await import("node:worker_threads");
    new Worker("../spammer/worker.js", {workerData: "hello"});
    // new Worker("../spammer/worker.js", {workerData: "hello1"});
    
}


main()


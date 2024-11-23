worker =  async () => {
    const {workerData} = await import("node:worker_threads");
    const {spammer} = await import("../spammer/output/SpammerUtils/index.js");
    const data = workerData;
    console.log(data);
    if (data == "here") {
      setInterval(()=> {
        console.log(data);
        }, 1000);
    }
}


worker()

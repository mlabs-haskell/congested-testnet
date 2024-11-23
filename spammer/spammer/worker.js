worker = async () => {
    const {workerData} = await import("node:worker_threads");
    const {spammer} = await import("../spammer/output/SpammerUtils/index.js");
    const {_new, write, read} = await import("../spammer/output/Effect.Ref/foreign.js");
    const data = workerData;
    s = _new({paidToWallets : false})();

    setInterval(() => {
     _s = read(s)();
     if (_s.paidToWallets){
       console.log("paid------------------------------------------------")
     }
    }, 10)

    spammer(s)()
}


worker()

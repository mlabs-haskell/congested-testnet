worker =  async () => {
    const {workerData} = await import("node:worker_threads");
    // const {spammer} = await import("../output/Spammer/index.js");
    // const {_new, write, read} = await import("../output/Effect.Ref/foreign.js");
    // const data = workerData;
    // s = _new({paidToWallets : false})();

    // setInterval(() => {
    //  _s = read(s)();
    //  if (_s.paidToWallets){
    //    console.log("paid------------------------------------------------")
    //  }
    // }, 10)

    // spammer(s)()
}


worker()

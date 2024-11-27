(async () => {
    const {Worker, workerData} = await import("node:worker_threads");
    // run spammers
  for (let iSpammer = 0; iSpammer < 2; iSpammer++ ){
    spammerData = {"iSpammer" : iSpammer}
    const spammer = new Worker("./src/spammer.js", {workerData: "hello"});
    spammer.on('message', (message) => {
          console.log(message);
        });
  }
    console.log("hello")
})()




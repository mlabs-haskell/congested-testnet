export const spammers =  async () => {
    const {spammer} = await import("../Spammer/index.js");
    const {Worker } = await import("node:worker_threads");
    new Worker("../spammer/worker.js", {workerData: "hello"});
  console.log("hello")
}




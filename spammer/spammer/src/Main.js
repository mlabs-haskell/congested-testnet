// exports.main = () => { console.log("hello")}  
export const main =  async () => {
    // const {spammer} = await import("../Spammer/foreign.js");
    const {Worker } = await import("node:worker_threads");
    new Worker("./workers.js", {workerData: "hello"});
    console.log("hello");
}

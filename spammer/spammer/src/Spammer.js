// const { parentPort } = require("node:worker_threads");
// export const sendMessageToMainThread = msg => () => {
//       parentPort.postMessage(msg)
// }
export const sendMessageToMainThread = parentPort => msg => {
      parentPort.postMessage(msg)
}
// const { parentPort } = require("node:worker_threads");
//
// exports.sendMessageToMainThread = (msg) => () => {
//   if (parentPort) {
//     parentPort.postMessage(msg);
//   } else {
//     console.error("No parentPort available in this thread.");
//   }
// };

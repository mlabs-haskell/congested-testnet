import * as csl from "@emurgo/cardano-serialization-lib-nodejs";

export const requestParent = parentPort => msg => () => { 
  parentPort.postMessage(msg);
  const promise = new Promise(resolve => {
    parentPort.on("message", resp => {resolve(resp)});
  }); 
  return promise;

}


// const waitUntilCondition = callbackConditionCheck => new Promise((resolve) => {
//     let loop = setInterval(() => {
//           if (callbackConditionCheck()) {
//             clearInterval(loop);
//             resolve()
//         };
//     }, 10);
// }
// );
//
// export const requestBackendPars =  eventEmitter => () => {
//   eventEmitter.emit('requestBackendPars');
//   const promise = new Promise((resolve) => {
//     eventEmitter.on("response")
//     // setInterval(() => {
//     //
//     // },10);
//   });
//   return promise; 
// };
//
// export const requestTx =  eventEmitter => () => {
//   eventEmitter.emit('requestTx');
// };
//
// export const postP = parentPort => reqMsg => respType => state => new Promise(async (resolve) => {
//   console.log("send")
//   parentPort.postMessage(reqMsg);
//   await waitUntilCondition(() => {console.log("finish");return (state.type == respType);})
//   resolve();
// })
//
// export const edHash = hex => csl.Ed25519KeyHash.from_hex(hex)
// export const pKey  =  hex => csl.PrivateKey.from_hex(hex)
// export const isEmptyState = state => {return Object.keys(state).length == 0;} 
//
// export const delState = state => () => { 
//  Object.keys(state).forEach(key => {
//     delete state[key];
//   });
//   console.log(state);
// };

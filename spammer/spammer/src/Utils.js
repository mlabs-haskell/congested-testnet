import * as csl from "@emurgo/cardano-serialization-lib-nodejs";

export const requestParent = parentPort => msg => () => { 
  parentPort.postMessage(msg);
  const promise = new Promise(resolve => {
    parentPort.once("message", resp => {resolve(resp)});
  }); 
  return promise;

}


export const edHash = hex => csl.Ed25519KeyHash.from_hex(hex)
export const pKey  =  hex => csl.PrivateKey.from_hex(hex)
// export const isEmptyState = state => {return Object.keys(state).length == 0;} 
//
// export const delState = state => () => { 
//  Object.keys(state).forEach(key => {
//     delete state[key];
//   });
//   console.log(state);
// };

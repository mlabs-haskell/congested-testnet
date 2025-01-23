import * as csl from "@emurgo/cardano-serialization-lib-nodejs";


// export const requestParent = parentPort => msg => () => { 
//   parentPort.postMessage(msg);
//   const promise = new Promise((resolve, reject) => {
//     const timer = setTimeout(() => {
//       reject(new Error("too long request from worker to parent"));
//     },3000);
//     parentPort.once("message", resp => {resolve(resp)});
//   }); 
//   return promise;
// }

export const waitPars = parentPort => () => { 
  const promise = new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      reject(new Error("too long request from worker to parent"));
    },20000);
    parentPort.once("message", resp => {resolve(resp)});
  }); 
  return promise;
}

// export const sendMsg = parentPort => msg => (
//   // new Promise((resolve) => (resolve(console.log(); parentPort.postMessage(msg))))
//   new Promise((resolve) => (resolve(console.log("hi"))))
// ); 
//

export const sendMsg = parentPort => msg => () => parentPort.postMessage(msg);


export const edHash = hex => csl.Ed25519KeyHash.from_hex(hex)
export const pKey = hex => csl.PrivateKey.from_hex(hex)
export const txHashToHex = txHash => txHash.to_hex() 
export const txHashFromHex = hex => csl.TransactionHash.from_hex(hex) 
export const getBackendPars = ()  => {
  return {
     mainWalletPath: process.env.WALLET_SKEY_PATH,
     ogmiosUrl: process.env.OGMIOS_URL,
     kupoUrl: process.env.KUPO_URL
  }; 
}; 


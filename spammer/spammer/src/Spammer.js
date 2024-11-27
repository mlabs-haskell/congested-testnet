//
// export const isAllowTransition = async () => allowTransition  
// export const isSpammerWalletsPaid = async () => spammerWalletsPaid 

export const sendMessage = async (msg) => {
      const {parentPort} = await import("node:worker_threads");
      parentPort.postMessage(msg)
}

// export const controlEnv = async () => {
//   const {parentPort} = await import("node:worker_threads");
//   var allowTransition = false ;
//   var spammerWalletsPaid = false ;
//   return {
//     "allowTransaction" :  async () => {
//        allowTransitionkkkkkkkkkkk
//        parentPort.postMessage(msg)
//     }
//   }
// } 

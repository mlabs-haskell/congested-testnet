export const paidToSpammerWalletsSuccess = obj => new Promise((resolve, reject) => {
      obj.parentPort.postMessage("successfullyPaidToSpammerWallet");
      obj.isAllowTransactions = false;
      resolve();
})

export const spammerId = obj => obj.spammerId; 

export const pauseSpammer = obj => new Promise((resolve) => {
    let loop_ = setInterval(
      () => {
          if (obj.isAllowTransactions) {
            clearInterval(loop_);
            resolve()
        };
    }, 10);
}
);

export const addTxHash = obj => txHash => () => {
  obj.txHash = txHash
  console.log(obj)
}; 

export const ed25519KeyHash = obj => () => obj.ed25519KeyHash;
export const allowTx = obj => obj.isAllowTransactions;

export const updateLastTime = dt => obj => () => {obj.awaitTxTime = dt};
export const isWaitTx = obj => (`waitTx` in obj) && (obj.waitTx); 

export const isPayFaucet = obj => () => (obj.ed25519KeyHash != null); 
export const sendTxHash = txHash => obj => () => {obj.txHash = txHash};
export const getIWallet = obj => () => obj.iWallet;
export const putIWallet = iW => obj => () => {obj.iWallet = iW};
export const resetKeyHash = obj => () => {obj.ed25519KeyHash = null;};
export const payLovelace = obj => () => obj.payLovelace;


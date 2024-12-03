export const paidToSpammerWalletsSuccess = obj => new Promise((resolve, reject) => {
      obj.parentPort.postMessage("successfullyPaidToSpammerWallet");
      resolve();
})

export const spammerId = obj => obj.spammerId; 

export const pauseSpammer = obj => new Promise((resolve) => {
    let loop_ = setInterval(
      () => {
          // console.log(`====== pause ======= spammerId : ${obj.spammerId}`)
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

export const ed25519KeyHash = obj => obj.ed25519KeyHash;
export const allowTx = obj => obj.isAllowTransactions;

export const updateLastTime = dt => obj => () => {obj.awaitTxTime = dt};
export const isWaitTx = obj => (`waitTx` in obj) && (obj.waitTx); 


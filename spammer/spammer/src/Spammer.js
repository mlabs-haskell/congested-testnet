export const paidToSpammerWalletsSuccess = obj => new Promise((resolve, reject) => {
      obj.parentPort.postMessage("successfullyPaidToSpammerWallet");
      obj.isAllowTransactions = false;
      resolve();
})


export const pauseSpammer = obj => new Promise((resolve, reject) => {
    let loop_ = setInterval(
      () => {
          if (obj.isAllowTransactions) {
            clearInterval(loop_);
            resolve()
        };
    }, 1000);
}
);

export const addTxHash = obj => txHash => () => {
  obj.txHash = txHash
  console.log(obj)
}; 

export const ed25519KeyHash = obj => obj.ed25519KeyHash;


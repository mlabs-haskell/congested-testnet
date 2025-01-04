const waitUntilCondition = callbackConditionCheck => new Promise((resolve) => {
    let loop = setInterval(() => {
          if (callbackConditionCheck()) {
            clearInterval(loop);
            resolve()
        };
    }, 10);
}
);

export const postP = parentPort => reqMsg => respType => state => new Promise(async (resolve) => {
  parentPort.postMessage(reqMsg);
  await waitUntilCondition(() => {return (state.type == respType);})
  resolve();
})

export const sTest = null; 

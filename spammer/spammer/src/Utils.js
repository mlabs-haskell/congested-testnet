const waitUntilCondition = callbackConditionCheck => new Promise((resolve) => {
    let loop = setInterval(() => {
          if (callbackConditionCheck()) {
            clearInterval(loop);
            resolve()
        };
    }, 10);
}
);

export const requestP = parentPort => reqMsg => respMsg => state => new Promise(async (resolve) => {
  parentPort.postMessage(reqMsg);
  await waitUntilCondition(() => {return (state.type == respMsg);})
  resolve();
})

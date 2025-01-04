const waitUntilCondition = callbackConditionCheck => new Promise((resolve) => {
    let loop = setInterval(() => {
          if (callbackConditionCheck()) {
            clearInterval(loop);
            resolve()
        };
    }, 10);
}
);
export const requestP = parentPort => msg => state => new Promise(async (resolve) => {
  parentPort.postMessage(msg);
  if (msg == "reqBackendPars") {
    await waitUntilCondition(() => {return (state.type == "respBackendPars");})
  };
  console.log("resolved")
  resolve();
})

// export const getResult = state => () => {console.log(state); return state}; 
// export const getResult = state => () => {console.log(state)}; 
export const getResult = state => () => {
  const {type, ...body} = state;
  if (type == "respBackendPars") {
    console.log(body);
    // return { constructor: "ResultEnvVars", value: body };
  };
}; 


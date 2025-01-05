import * as csl from "@emurgo/cardano-serialization-lib-nodejs";

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

export const edHash = hex => csl.Ed25519KeyHash.from_hex(hex)
export const pKey  =  hex => csl.PrivateKey.from_hex(hex)

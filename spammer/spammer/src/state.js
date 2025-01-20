const fs = require("fs");
const path = require("path");
const utils = require(path.resolve(__dirname, "./utils.js"));
const scripts = require(path.resolve(__dirname, "./scripts.js"));

const state = loadState();

const generateDefaultState = (keys) => ({
  // workers wallets
  walletsKeys: keys, 
  walletsI : 0,
  walletsStatus : "EMPTY", // also "INPROGRESS" and "FILLED"
  // locked transactions [[txHash, script]]
  locked: {},
  // faucet {payKeyHash : "pay" | "inprogress" | if paid then txHash for paid transaction}
  faucet: {}
});
// const mainWalletPath: process.env.WALLET_SKEY_PATH,
// const ogmiosUrl: process.env.OGMIOS_URL,
// const kupoUrl: process.env.KUPO_URL,

var memPoolSize; 
// metric for prometheus
var awaitTxTime; 
// pause workers 
var flagPause = false;
var flagSendBackendPars = false;

/** 
  * possible stateUpdates on messages from workers: 
  * messages:
  * backendPars -> flagSendBackendPars = true 
  * initializedWallets -> flagWalletsFilled = true set wallets filled, unpause
  * locked > 100 txs -> unlock
  * pay or lock ->
  */ 

/** 
  * possible messages from current state: 
  * flagSendBackendPars -> send backendPars
  * if not flagWalletsFilled  -> pay to all wallets from main wallet
  * initializedWallets -> flagWalletsFilled = true set wallets filled, unpause
  * locked > 100 txs -> unlock
  * pay or lock ->
  */ 

const message = () => {
    if (flagSendBackendPars)
      return {
      };
    if (flagPause) return {};
    if (!state.walletsFilled) return {
    };
};  


const nextState = (responseFromWorker) => {
  switch (state.type) {
    case "ZERO": 
      if (responseFromWorker == "initializedWallets") {
        state.type = "PAY";
      }
      break
    case "PAY":
  };

};

// send backend pars to workers
const backendPars = () => ({
  type: "BackendPars",
  ogmiosUrl: state.ogmiosUrl,
  kupoUrl: state.kupoUrl,
  walletPath: state.mainWalletPath
});

// tx params sent to workers 
const txPars = () => {
  if (!state.walletsFilled){
    return payToWallets();
  }
  if (!state.walletsFilled){
    return payToWallets();
  }
  // is there request for faucet
  const faucetPubKeyHex = _faucetPayKey();
  // pay to faucet is 1st priority
  if (faucetPubKeyHex && !state.wallets.empty) {
    const response = {
      tx: "pay",
      pars: {
        key: walletKey(),
        hash: faucetPubKeyHex,
        amount: "1000000000",
      },
    };
    nextWallet();
    return response;
  }

  if (state.await && state.pause) {
    return { tx: "pause", pars: {} };
  }

  if (state.wallets.empty) {
    state.pause = true;
    state.await = true;
    return {
      tx: "initWallets",
      pars: {
        hashes: state.wallets.hashes,
        amount: "1000000000000000",
        await: true,
      },
    };
  }

  let response;
  if (state.tx.locked.txHashScript.length > 100) {
    const [txHash, script] = state.tx.locked.txHashScript.shift();
    state.tx.locked.txHashScript.push([txHash, script]);
    response = {
      tx: "unlock",
      pars: {
        key: walletKey(),
        script,
        lockedTxHash: txHash,
      },
    };
  } else {
    const script = scripts.script();
    if (script === "") {
      response = {
        tx: "pay",
        pars: {
          key: walletKey(),
          hash: walletHash(),
          amount: "3000000",
        },
      };
    } else {
      response = {
        tx: "lock",
        pars: {
          key: walletKey(),
          script,
          amount: "3000000",
        },
      };
    }
  }

  nextWallet();
  response.pars.await = !state.await;
  state.await = true;
  return response;
};

const generateInitialWalletState = (keys) => ({
  keys,
  ikey: 0,
  hashes: keys.map(utils.hash),
  ihash: 1,
  empty: true,
});

const setEnvVarsInState = (state) => {
  state.mainWallet.path = process.env.WALLET_SKEY_PATH;
  state.ogmiosUrl = process.env.OGMIOS_URL;
  state.kupoUrl = process.env.KUPO_URL;

}; 


const loadState = () => {
  if (fs.existsSync(process.env.SPAMMER_STATE_FILE)) {
    const fileContent = fs.readFileSync(process.env.SPAMMER_STATE_FILE, "utf-8");
    const parsedState = JSON.parse(fileContent);
    parsedState.pause = false;
    parsedState.await = false;
    setEnvVarsInState(parsedState);
    return parsedState;
  }
  return generateDefaultState(utils.generatePkeys(300));
};


const saveState = () => {
  fs.writeFileSync(process.env.SPAMMER_STATE_FILE, JSON.stringify(state));
};

const handleExitSignals = () => {
  ["SIGINT", "SIGTERM", "SIGQUIT"].forEach((signal) =>
    process.on(signal, () => {
      saveState();
      process.exit();
    })
  );
};

const handleUncaughtErrors = () => {
  process.on("uncaughtException", (error) => {
    console.error("Uncaught exception:", error);
    console.log("Saving state before exit due to uncaught exception...");
    saveState();
    process.exit(1);
  });

  process.on("unhandledRejection", (reason) => {
    console.error("Unhandled promise rejection:", reason);
    console.log("Saving state before exit due to unhandled rejection...");
    saveState();
    process.exit(1);
  });
};

handleExitSignals();
handleUncaughtErrors();

const nextIndex = (i, length) => (i === length - 1 ? 0 : i + 1);

const walletKey = () => state.wallets.keys[state.wallets.ikey];
const walletHash = () => state.wallets.hashes[state.wallets.ihash];
const nextWallet = () => {
  state.wallets.ikey = nextIndex(state.wallets.ikey, state.wallets.keys.length);
  state.wallets.ihash = nextIndex(state.wallets.ihash, state.wallets.hashes.length);
};


const faucetPay = (hex) => {
  state.faucet[hex] = "pay";
};

const _faucetPayKey = () => {
  for (const [key, status] of Object.entries(state.faucet)) {
    if (status === "pay") {
      state.faucet[key] = "inprogress";
      return key;
    }
  }
  return null;
};

const faucetPaid = (pKeyHash, txHash) => {
  if (Object.hasOwn(state.faucet, pKeyHash)) state.faucet[pKeyHash] = txHash;
};

const faucetFinish = (pKeyHash) => {
  txHash = state.faucet[pKeyHash]
  if (txHash && txHash.length == 64) { 
    delete state.faucet[pKeyHash];
    return txHash
  };
  return null
}; 

const pushLocked = (script, txHash) => {
  state.tx.locked.txHashScript.push([txHash, script]);
};

const clearLocked = (txHash) => {
  state.tx.locked.txHashScript = state.tx.locked.txHashScript.filter(arr => arr[0] !== txHash);
};


module.exports = {
  backendPars,
  getState: () => state,
  pause: () => (state.pause = true),
  unpause: () => (state.pause = false),
  isWalletsEmpty: () => state.wallets.empty,
  walletsHashes: () => state.wallets.hashes,
  mainWalletFree: () => state.mainWallet.free,
  setMainWalletBusy: () => (state.mainWallet.free = false),
  setWalletsInitiated: () => (state.wallets.empty = false),
  noAwaitTxToMeasureTime: () => (state.await = false),
  txPars,
  pushLocked,
  clearLocked,
  faucetPay,
  faucetPaid,
  faucetFinish,
  awaitTxTime: () => state.awaitTxTime,
  setAwaitTxTime: (t) => (state.awaitTxTime = t),
};

const fs = require("fs");
const path = require("path");
const utils = require(path.resolve(__dirname, "./utils.js"));
const scripts = require(path.resolve(__dirname, "./scripts.js"));

const generateInitialWalletState = (keys) => ({
  keys,
  ikey: 0,
  hashes: keys.map(utils.hash),
  ihash: 1,
  empty: true,
});

const generateDefaultState = (keys) => ({
  mainWallet: {
    path: process.env.WALLET_SKEY_PATH,
    free: true,
  },
  ogmiosUrl: process.env.OGMIOS_URL,
  kupoUrl: process.env.KUPO_URL,
  wallets: generateInitialWalletState(keys),
  tx: {
    // transactions types like  "pay" | "lock" | "unlock"
    type: "pay",
    // [[txHash, script]]
    locked: { txHashScript: [] },
  },
  // pause workers 
  pause: false,
  // use await flag awaiting tx for time delay 
  await: false,
  awaitTxTime: "0.0",
  // faucet {payKeyHash : "pay" | "inprogress" | if paid then txHash for paid transaction}
  faucet: {},
});

const loadState = () => {
  if (fs.existsSync(process.env.SPAMMER_STATE_FILE)) {
    const fileContent = fs.readFileSync(process.env.SPAMMER_STATE_FILE, "utf-8");
    const parsedState = JSON.parse(fileContent);
    parsedState.pause = false;
    parsedState.await = false;
    return parsedState;
  }
  return generateDefaultState(utils.generatePkeys(300));
};

const state = loadState();

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

const backendPars = () => ({
  type: "BackendPars",
  ogmiosUrl: state.ogmiosUrl,
  kupoUrl: state.kupoUrl,
  walletPath: state.mainWallet.path,
});

const faucetPay = (hex) => {
  state.faucet[hex] = "pay";
};

const _faucetPay = () => {
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

const txPars = () => {
  const faucetPubKeyHex = _faucetPay();
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

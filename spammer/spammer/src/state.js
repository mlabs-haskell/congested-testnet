const fs = require("fs");
const path = require("path");
const { util } = require("webpack");
const utils = require(path.resolve(__dirname, "./utils.js"));
const scripts = require(path.resolve(__dirname, "./scripts.js"));

state = loadState();

function loadState() {
  if (fs.existsSync(process.env.SPAMMER_STATE_FILE)) {
    const fileContent = fs.readFileSync(process.env.SPAMMER_STATE_FILE, "utf-8");
    const parsedState = JSON.parse(fileContent);
    return parsedState;
  }
  return generateDefaultState();
};

function generateDefaultState () {
  return {
  // workers wallets
  walletsKeys: utils.generatePkeys(200),  
  walletsEmpty : true, 
  // locked transactions : [[txHash, script]]
  locked: [], 
  // faucet {payKeyHash : "pay" | "inprogress" | if paid then txHash for paid transaction}
  faucet: {}
  };
};

// temporary state vars
let KEY_IND = 0;

const initializeWalletsPars = () => ({
  tx: "initWallets",
  pars: {
    hashes: state.walletsKeys.map(utils.hash),
    amount: "1000000000000000"
  }
});

const handleMessage = msg => {
  const msgParts = msg.split("_");
  const txHash = msgParts[msgParts.length - 1];
  const header = msgParts[0]

  if (header == "initWallets") {
    state.walletsEmpty = false;
    return txHash;
  };

  if (header == "locked") {
    const script = msgParts[1];
    state.locked.push([txHash, script]);
    return txHash;
  }; 

  if (header == "locked") {
    const script = msgParts[1];
    state.locked.push([txHash, script]);
    return txHash;
  }; 

  if (header == "unlocked") {
    const lockedTxHash = msgParts[1];
    state.locked = state.locked.filter(arr => arr[0] !== lockedTxHash);
    return txHash;
  }; 

};

const txPars = () => {
  const key = state.walletsKeys[KEY_IND];
  KEY_IND += 1;
  if (KEY_IND == state.walletsKeys.length) KEY_IND = 0;

  //unlock
  if (state.locked.length > 100) {
    const [txHash, script] = state.locked.shift();
    // push to the end, and in case of fail return back to this txHash later
    state.locked.push([txHash, script])
    return {
      tx: "unlock",
      pars: {
        key : key,
        script : script,
        lockedTxHash : txHash
      }
    };
  }; 

  // choose script or just payment transaction
  const script = scripts.script();

  // pay
  if (script == "") {
    return {
      tx: "pay",
      pars: {
        key: key,
        hash: utils.hash(key),
        amount: "3000000",
      }
    };
  }; 

  // lock 
    return {
      tx: "lock",
      pars: {
        key: key,
        script: script,
        amount: "3000000",
      }
    };
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

const walletsEmpty = () => (state.walletsEmpty);
const setWalletsInitialized = () => {state.walletsEmpty = false;};

module.exports = {
  walletsEmpty,
  initializeWalletsPars,
  setWalletsInitialized,
  txPars,
  handleMessage
};

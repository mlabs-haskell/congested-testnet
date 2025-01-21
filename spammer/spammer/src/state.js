const fs = require("fs");
const path = require("path");
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
  // locked transactions [[txHash, script]]
  locked: {},
  // faucet {payKeyHash : "pay" | "inprogress" | if paid then txHash for paid transaction}
  faucet: {}
  };
};

const initializeWalletsPars = () => ({
  tx: "initWallets",
  pars: {
    hashes: state.walletsKeys.map(utils.hash),
    amount: "1000000000000000"
  }
});

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
  setWalletsInitialized
};

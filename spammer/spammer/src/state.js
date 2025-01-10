const fs = require("fs");
const path = require("path");
const utils = require(path.resolve(__dirname, "./utils.js"));
const scripts = require(path.resolve(__dirname, "./scripts.js"));
const keys = utils.generatePkeys(200);

const load = () => {
  if (fs.existsSync(process.env.SPAMMER_STATE_FILE)) {
    const fileContent = fs.readFileSync(process.env.SPAMMER_STATE_FILE, 'utf-8');
    const parsedState = JSON.parse(fileContent);
    // unpause of spammer if state saved with true
    parsedState.pause = false;
    return parsedState;
  };
  // default state
  return  {
    mainWallet : {
      path : process.env.WALLET_SKEY_PATH,
      free: true 
    },
    ogmiosUrl : process.env.OGMIOS_URL,
    kupoUrl : process.env.KUPO_URL,

    // generate 200 spammer wallets in order to use different keys and not wait until tx is finished  
    // which is necessary for spamming approach
    wallets : {
      // bech32
      keys : keys, 
      ikey : 0,
      hashes: keys.map(utils.hash),
      ihash : 1,
      // if false, need take funds from main wallet
      empty: true, 
      // current index
    },
    tx : {
      // "pay" | "lock" | "unlock"
      type : "pay", 
      lockedUtxos : []
    },
    pause : false };

};

const state = load(); 


['SIGINT', 'SIGTERM', 'SIGQUIT'].forEach(signal => process.on(signal, () => {
    fs.writeFileSync(process.env.SPAMMER_STATE_FILE, JSON.stringify(state));
    process.exit();
  }));

process.on('uncaughtException', (error) => {
    console.error('Uncaught exception:', error);
    console.log('Saving state before exit due to uncaught exception...');
    fs.writeFileSync(process.env.SPAMMER_STATE_FILE, JSON.stringify(state));
    process.exit(1); 
});

process.on('unhandledRejection', (reason, promise) => {
    console.error('Unhandled promise rejection:', reason);
    console.log('Saving state before exit due to unhandled rejection...');
    fs.writeFileSync(process.env.SPAMMER_STATE_FILE, JSON.stringify(state));
    process.exit(1); 
});

const nextI = i => {
  if (i == state.wallets.keys.length - 1){
    return 0;
  };
  return i+1;
};

const walletKey = () => state.wallets.keys[state.wallets.ikey];
const walletHash = () => state.wallets.hashes[state.wallets.ihash];


const nextWallet = () => {
  state.wallets.ikey = nextI(state.wallets.ikey)
  state.wallets.ihash = nextI(state.wallets.ihash)
};

const backendPars = () => { return {
         type : "BackendPars",
         ogmiosUrl : state.ogmiosUrl,
         kupoUrl : state.kupoUrl,
         walletPath : state.mainWallet.path
};};

const setWalletsInitiated = () => {
       state.wallets.empty = false;
}; 

const getState = () => state; 
const isPause = () => state.pause; 
const setPause = () => {state.pause = true;}; 
const setUnPause = () => {state.pause = false;}; 


const isWalletsEmpty = () => state.wallets.empty; 
const walletsHashes= () => state.wallets.hashes; 

const mainWalletFree = () => state.mainWallet.free; 
const setMainWalletBusy = () => {state.mainWallet.free = false;}; 

const txPars = () => {
  if (isPause()) {
    return {
      tx : "pause",
      pars : {
      }
    };
  } else if (isWalletsEmpty()) {
    // initialize spammer wallets
    // set pause because use mainWallet
    setPause();
    return {
      tx : "initWallets",
      pars : {
        hashes : walletsHashes(),
        amount : "1000000000000000"
      }
    };
  } else {
    const resp = {
      tx : "pay",
      pars : {
        key : walletKey(),
        hash : walletHash(),
        amount : "3000000" 
      }
    };
    nextWallet();
    return resp;
  };
};



module.exports = {
  walletKey,
  walletHash,
  nextWallet, 
  backendPars,
  getState,
  setPause,
  setUnPause,
  isPause,
  isWalletsEmpty,
  walletsHashes,
  mainWalletFree,
  setMainWalletBusy,
  setWalletsInitiated,
  txPars
};


const fs = require("fs");
const path = require("path");
const utils = require(path.resolve(__dirname, "./utils.js"));
const scripts = require(path.resolve(__dirname, "./scripts.js"));
const keys = utils.generatePkeys(300);



const load = () => {
  // upload state from file if exists
  if (fs.existsSync(process.env.SPAMMER_STATE_FILE)) {
    const fileContent = fs.readFileSync(process.env.SPAMMER_STATE_FILE, 'utf-8');
    const parsedState = JSON.parse(fileContent);
    // unpause of spammer if state saved with true
    parsedState.pause = false;
    parsedState.await = false;
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
      // transactions types like  "pay" | "lock" | "unlock"
      type : "pay", 
      // locked  transactions
      locked : {
        txHashScript : []
      }, 

    },
    pause : false, 
    // use await to awaiting tx for time measure
    await : false  
  };

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
const noAwaitTxToMeasureTime = () => {state.await = false;}; 
const pause = () => {state.pause = true;}; 
const unpause = () => {state.pause = false;}; 


const isWalletsEmpty = () => state.wallets.empty; 
const walletsHashes= () => state.wallets.hashes; 

const mainWalletFree = () => state.mainWallet.free; 
const setMainWalletBusy = () => {state.mainWallet.free = false;}; 

function pushLocked(script, txHash) {
  state.tx.locked.txHashScript.push([txHash, script]);
};

function clearLocked(txHash) {
  state.tx.locked.txHashScript = state.tx.locked.txHashScript.filter(arr => arr[0] != txHash);
};


const txPars = () => {
  if (state.await && state.pause){
    return {
      tx : "pause",
      pars : {}
    };
  } 

  if (isWalletsEmpty()) {
    // initialize spammer wallets
    // set pause because use mainWallet
    pause();
    state.await = true;
    return {
      tx : "initWallets",
      pars : {
        hashes : walletsHashes(),
        amount : "1000000000000000",
        await : true 
      }
    };
  }; 
  
  let resp;

  //unlock
  if (state.tx.locked.txHashScript.length > 100) {
    // move unlock tx data to the end, to clear it after successfull submission 
    let [txHash, script] = state.tx.locked.txHashScript.shift();
    state.tx.locked.txHashScript.push([txHash, script]);
    resp = {
      tx : "unlock",
      pars : {
        key : walletKey(),
        script : script ,
        lockedTxHash : txHash
      }
    };
  } else {
    let script = scripts.script();

    // pay
    if (script == "") {
      resp = {
        tx : "pay",
        pars : {
          key : walletKey(),
          hash : walletHash(),
          amount : "3000000" 
        }
      };
    } else {

      // lock
      resp = {
        tx : "lock",
        pars : {
          key : walletKey(),
          script : script,
          amount : "3000000" 
        }
      };
    };
  };
  nextWallet();

  resp.pars.await = false;
  if (!state.await) {
    state.await = true;
    resp.pars.await = true;
  };

  return resp;
  };



module.exports = {
  walletKey,
  walletHash,
  nextWallet, 
  backendPars,
  getState,
  pause,
  unpause,
  isWalletsEmpty,
  walletsHashes,
  mainWalletFree,
  setMainWalletBusy,
  setWalletsInitiated,
  noAwaitTxToMeasureTime,
  txPars,
  pushLocked,
  clearLocked
};


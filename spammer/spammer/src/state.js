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
  walletsKeys: [],  
  walletsEmpty : true, 
  walletsInd : 0,
  // locked transactions : [[txHash, script]]
  locked: [], 
  // faucet {payKeyHash : "pay" | "inprogress" | if paid then txHash for paid transaction}
  faucet: {}
  };
};

var newWalletsKeys;
const initializeWalletsPars = () => {
  newWalletsKeys = utils.generatePkeys(250); 
  return {
  tx: "initWallets",
  pars: {
    hashes: newWalletsKeys.map(utils.hash),
    amount: "1000000000000000"
  }
  };
};


const nextKey = () => {
  state.walletsInd += 1;
  if (state.walletsInd  == state.walletsKeys.length) state.walletsInd = 0;
};

const faucetPayPars = txHash => {
  const key = state.walletsKeys[state.walletsInd];
  nextKey();
    return {
      tx: "faucet",
      pars: {
        key: key,
        hash: txHash,
        amount: "10000000000"
      }
    };
};



let faucetTx = {};
const faucetTxHash = pubKeyHashHex => {
  const txHash =  faucetTx[pubKeyHashHex];
  if (txHash) {
    delete faucetTx[pubKeyHashHex];
  }; 
  return txHash
}

const handleMessage = msg => {
  const msgParts = msg.split("_");
  const txHash = msgParts[msgParts.length - 1];
  const header = msgParts[0]

  if (header == "error") {
    console.log(msg);
  }; 


  if (header == "initWallets") {
    state.walletsKeys.push(...newWalletsKeys);
    newWalletsKeys = undefined;
    // mark only if enough workers wallets are processed 
    if (state.walletsKeys.length > 499) {
      state.walletsEmpty = false;
    }
    return txHash;
  };

  if (header == "paid") {
    return txHash;
  }; 

  if (header == "faucet") {
    const pubKeyHash = msgParts[1];
    faucetTx[pubKeyHash] = txHash;
    return txHash;
  }; 

  if (header == "locked") {
    const script = msgParts[1];
    state.locked.push([txHash, script]);
    return txHash;
  }; 

  if (header == "unlocked") {
    const lockedTxHash = msgParts[1];
    console.log(`locked tx number : ${state.locked.length}`)
    state.locked = state.locked.filter(arr => arr[0] !== lockedTxHash);
    return txHash;
  }; 

};


// spammer loop pars
const txPars = () => {

  const key = state.walletsKeys[state.walletsInd];
  nextKey();

  //unlock
  if (state.locked.length > 100) {
    const [txHash, script] = state.locked.shift();
    state.locked.push([txHash,script]);

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
      amount: "3000000"
    }
  };
};

const saveState = () => {
  fs.writeFileSync(process.env.SPAMMER_STATE_FILE, JSON.stringify(state));
};


const walletsEmpty = () => (state.walletsEmpty);

module.exports = {
  walletsEmpty,
  initializeWalletsPars,
  txPars,
  handleMessage,
  saveState,
  faucetPayPars,
  faucetTxHash
};

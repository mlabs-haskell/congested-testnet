const path = await import("path");
const utils = await import(path.resolve(__dirname, "./utils.js"));
const scripts = await import(path.resolve(__dirname, "./scripts.js"));
const keys = utils.generatePkeys(200);

const state = {
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
      hashes: keys.map(utils.hash),
      // if false, need take funds from main wallet
      empty: true, 
      // current index
      i : 0
    },
    tx : {
      // "pay" | "lock" | "unlock"
      type : "pay", 
      lockedUtxos : []
    },
  };

export const nextWallet = () => {
  let w = state.wallets;
  if (w.i > w.keys.length) {

  };
};

export const getState = () => state; 

export const walletsEmpty = () => state.wallets.empty; 
export const walletsHashes= () => state.wallets.hashes; 

export const mainWalletFree = () => state.mainWallet.free; 
export const setMainWalletBusy = () => {state.mainWallet.free = false;}; 


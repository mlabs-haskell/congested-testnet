module Spammer.Types where

import Contract.Prelude

import Contract.Value (Value)
import Contract.Wallet (KeyWallet)
import Ctl.Internal.Types.Scripts (Validator)

newtype SpammerEnv = SpammerEnv {
  walletFrom :: Maybe KeyWallet , 
  walletTo :: Maybe KeyWallet ,  
  validatorTo :: Maybe Validator ,
  validatorFrom :: Maybe Validator , 
  valueToPay :: Maybe Value, 
  test :: Maybe String
  } 

derive instance Newtype SpammerEnv _



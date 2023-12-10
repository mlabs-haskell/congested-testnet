module Spammer.State.Types where

import Contract.Prelude

import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Contract.Wallet (KeyWallet)
import Ctl.Internal.Types.Scripts (Validator)

newtype SpammerEnv = SpammerEnv
  { wallet :: Maybe KeyWallet
  , validator :: Maybe Validator
  , value :: Maybe Value
  , utxos :: Maybe UtxoMap
  }

defaultSpammerEnv = SpammerEnv { wallet: Nothing, validator: Nothing, value: Nothing, utxos: Nothing }

derive instance Newtype SpammerEnv _


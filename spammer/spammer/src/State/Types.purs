module Spammer.State.Types where

import Contract.Prelude

import Contract.Value (Value)
import Contract.Wallet (KeyWallet)
import Ctl.Internal.Types.Scripts (Validator)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Sequence (Seq, empty)

newtype SpammerEnv = SpammerEnv
  { wallet :: Maybe KeyWallet
  , validator :: Maybe (Validator /\ String)
  , value :: Maybe Value
  , txInputsUsed :: Seq TransactionInput
  , counter :: Int
  , addUtxo :: Boolean
  }

defaultSpammerEnv :: SpammerEnv
defaultSpammerEnv = SpammerEnv
  { wallet: Nothing
  , validator: Nothing
  , value: Nothing
  , txInputsUsed: empty
  , counter : 1 
  , addUtxo : false
  }

derive instance Newtype SpammerEnv _


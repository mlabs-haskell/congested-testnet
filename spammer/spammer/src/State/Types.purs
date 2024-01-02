module Spammer.State.Types where

import Contract.Prelude

import Contract.TxConstraints (InputWithScriptRef)
import Contract.Utxos (UtxoMap)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Sequence (Seq, empty)

type TxLocked = TransactionInput /\ InputWithScriptRef


newtype SpammerEnv = SpammerEnv
  { 
    txInputsUsed :: Seq TransactionInput
  , counter :: Int
  , addUtxo :: Boolean
  , txLocked :: Seq UtxoMap 
  }

defaultSpammerEnv :: SpammerEnv
defaultSpammerEnv = SpammerEnv
  { 
  txInputsUsed: empty
  , counter: 1
  , addUtxo: false
  , txLocked : empty
  }

derive instance Newtype SpammerEnv _


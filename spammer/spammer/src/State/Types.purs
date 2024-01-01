module Spammer.State.Types where

import Contract.Prelude

import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Sequence (Seq, empty)

newtype SpammerEnv = SpammerEnv
  { 
  txInputsUsed :: Seq TransactionInput
  , counter :: Int
  , addUtxo :: Boolean
  , txLocked :: Seq TransactionInput
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


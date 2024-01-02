module Spammer.State.Types where

import Contract.Prelude

import Contract.TxConstraints (InputWithScriptRef)
import Contract.Utxos (UtxoMap)
import Ctl.Internal.Types.Transaction (TransactionInput)
import Data.Sequence (Seq, empty)

type TxLocked = TransactionInput /\ InputWithScriptRef

newtype SpammerEnv = SpammerEnv
  { txInputsUsed :: Seq TransactionInput
  , numberUtxos :: Int
  , txLocked :: Seq UtxoMap
  }

defaultSpammerEnv :: SpammerEnv
defaultSpammerEnv = SpammerEnv
  { txInputsUsed: empty
  , numberUtxos: 0
  , txLocked: empty
  }

derive instance Newtype SpammerEnv _


module Spammer.Query.Utxos where

import Contract.Prelude
import Contract.Utxos (UtxoMap)

type Result = Array { pkey :: String }

getUtxos' :: Aff (Maybe UtxoMap)
getUtxos' = pure Nothing

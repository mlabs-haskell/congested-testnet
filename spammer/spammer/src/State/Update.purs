module Spammer.State.Update where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract, liftedM)
import Contract.Scripts (Validator, validatorHash)
import Contract.Utxos (UtxoMap, utxosAt)
import Control.Monad.Cont (lift)
import Control.Monad.State (StateT, get, modify_)
import Data.Map as Map
import Data.Maybe (Maybe(..))


getUtxoFromValidator :: Validator -> Contract UtxoMap
getUtxoFromValidator val = do
  let
    valHash = validatorHash val
    address = scriptHashAddress valHash Nothing
  utxosAt address


module Spammer.Query.Scripts where

import Contract.Prelude

import Contract.Scripts (Validator)
import Contract.Transaction (plutusV2Script)
import Data.Maybe (Maybe)
import Spammer.Query.Utils (decodeCborHexToBytes)

foreign import _sampleValidator :: Effect String
foreign import _allValidators :: Effect (Array String)

strToValidator :: String -> Maybe Validator
strToValidator str = do
  bytes <- decodeCborHexToBytes str
  pure <<< wrap <<< plutusV2Script $ bytes

sampleValidator :: Effect (Maybe Validator)
sampleValidator = do
  str <- _sampleValidator
  if str == "" then pure Nothing
  else pure $ strToValidator str

allValidators :: Effect (Maybe (Array Validator))
allValidators = do
  xs <- _allValidators
  pure $ traverse strToValidator xs


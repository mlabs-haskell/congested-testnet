module Spammer.Db (executeQuery, KeysRow) where

import Contract.Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type KeysRow = {pkey :: String, pubkey :: String} 

foreign import _executeQuery :: String -> EffectFnAff (Array KeysRow)

executeQuery :: String -> Aff (Array KeysRow) 
executeQuery = fromEffectFnAff <<< _executeQuery 

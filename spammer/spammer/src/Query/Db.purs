module Spammer.Db (executeQuery) where

import Contract.Prelude

import Data.Argonaut (Json)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type Query = String

foreign import _executeQuery :: Query -> EffectFnAff Json

executeQuery :: Query -> Aff Json
executeQuery = fromEffectFnAff <<< _executeQuery


-- module Spammer.Prometheus (getAvgMemPoolUsage) where
module Spammer.Prometheus where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import _queryAvgMemPoolUsage :: EffectFnAff Number

getAvgMemPoolUsage :: Aff Number
getAvgMemPoolUsage = fromEffectFnAff _queryAvgMemPoolUsage


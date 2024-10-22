module Spammer where

import Contract.Prelude
import Spammer.Config

import Contract.Monad (launchAff_, runContract)

-- example :: ContractParams -> Effect Unit
-- example cfg = launchAff_ $ do
--   runContract cfg contract

main :: Effect Unit
main = do
  log "Hi"
  envVars <- getEnvVars
  log envVars.kupoUrl
  log envVars.walletPath
  let cfg = config envVars 
  launchAff_ $ do
     runContract cfg do 
        log "hi"

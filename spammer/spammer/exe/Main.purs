-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)

main :: Effect Unit
main = do 
  log "hi"
  launchAff_ do
     log "hi"
       
  

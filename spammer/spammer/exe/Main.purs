-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module Main (main) where

import Contract.Prelude

main :: Effect Unit
main = do 
 log "hi"

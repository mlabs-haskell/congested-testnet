module Faucet where

import Contract.Prelude

import Contract.Monad (launchAff_, runContract)
import Control.Monad.Error.Class (try)
import Effect.Aff (joinFiber, launchAff)
import Node.Encoding (Encoding(..))
import Node.HTTP (Request, Response, createServer, listen, requestAsStream, requestMethod, responseAsStream)
import Node.Stream (end, onDataString, writeString)
import Spammer.Config (config)
import Spammer.Contracts.Faucet (payToAddress)

type Result = { pubKeyHashHex :: String }

foreign import strToResult :: String -> Effect Result

faucet :: Request -> Response -> Effect Unit
faucet req resp =
  let
    reqStream = requestAsStream req
    respStream = responseAsStream resp
    method = requestMethod req
    faucet_config = config "/wallet/wallet.skey" "ogmios.local" "kupo.local" 1337 1442
  in
    if method == "POST" then
      onDataString reqStream UTF8 \str -> do
        fiber <- launchAff $ try do
          body <- liftEffect $ strToResult str
          runContract faucet_config (payToAddress body.pubKeyHashHex)
        fiber1 <- launchAff do
          txHash <- joinFiber fiber
          liftEffect $ writeString respStream UTF8 ("\n" <> show txHash <> "\n") (log $ show txHash)
        launchAff_ do
          _ <- joinFiber fiber1
          liftEffect $ end respStream (log "end response")

    else do
      _ <- writeString respStream UTF8 ("\n" <> "must be POST request with JSON body" <> "\n") (log "not POST request")
      end respStream (log "end response")

main :: Effect Unit
main = do
  server <- createServer faucet
  let
    backlog = Nothing
    hostname = "0.0.0.0"
    port = 8000
    options = { backlog, hostname, port }
  listen server options (log "start faucet")


module Spammer.Contracts.GetFunds where

import Contract.Prelude

import Aeson (encodeAeson, toStringifiedNumbersJson)
import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (string)
import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (launchAff_)
import Data.HTTP.Method (Method(..))
import Spammer.Keys (getHexFromEd25519Hash)


getFundsFromFaucet :: PaymentPubKeyHash -> Effect Unit
getFundsFromFaucet pkh  = launchAff_ do
  let
    hex = getHexFromEd25519Hash <<< unwrap <<< unwrap $ pkh 
    req = defaultRequest {
                          url = "http://congested-testnet.staging.mlabs.city:8000",
                          method = Left POST,
                          responseFormat = string, 
                          headers = [(ContentType $ wrap "application/json")],
                          content = Just <<< Json <<< toStringifiedNumbersJson $ encodeAeson $ {pubKeyHashHex : hex}  
                         }
  _ <- request req
  pure unit




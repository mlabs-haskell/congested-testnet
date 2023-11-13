module Main where
import AlwaysTrueScript
    ( script, AlwaysTrueScriptParams(AlwaysTrueScriptParams, size), alwaysSucceedsCompiled )
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as BS
import Cardano.Api (writeFileTextEnvelope, TextEnvelopeDescr,  SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV2), Script, PlutusScriptVersion (PlutusScriptV2), PlutusScriptV2, Script)
import Data.String (IsString(fromString))
import Data.ByteString (toStrict)
import PlutusLedgerApi.Common (serialiseCompiledCode)


main :: IO ()
main = do 
  let fpath = fromString "validator.plutus"
      scriptName :: Cardano.Api.TextEnvelopeDescr = fromString "always-true"
  print scriptLength
  case Cardano.Api.deserialiseFromCBOR (Cardano.Api.AsScript Cardano.Api.AsPlutusScriptV2) serialisedScriptByteString  of
    Left err -> print err
    Right script'' ->
      Cardano.Api.writeFileTextEnvelope @(Script PlutusScriptV2) fpath (Just scriptName) script''
      >>= either print return
    where
      script' = alwaysSucceedsCompiled -- script params
      serialisedScript = serialiseCompiledCode script'
      serialisedScriptByteString = BS.fromShort serialisedScript
      scriptLength = B.length serialisedScriptByteString
      params =
       AlwaysTrueScriptParams { size = 2}




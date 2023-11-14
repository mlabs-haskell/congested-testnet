module Main where
import AlwaysTrueScript
    ( script, AlwaysTrueScriptParams(AlwaysTrueScriptParams, size), alwaysSucceedsCompiled )
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as BS
import Cardano.Api (writeFileTextEnvelope,  SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV2), Script, PlutusScriptVersion (PlutusScriptV2), PlutusScriptV2, Script)
import Plutus.V2.Ledger.Api (fromCompiledCode)
import Codec.Serialise (serialise)

scriptToCBOR = B.toStrict . serialise . fromCompiledCode



main =
  case deserialiseFromCBOR (AsScript AsPlutusScriptV1) $ scriptToCBOR alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV2) "always-succeeds.plutus" (Just "My script") script
      >>= either print return


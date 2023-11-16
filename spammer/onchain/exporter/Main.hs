module Main where
import AlwaysTrueScript
    ( ser,  AlwaysTrueScriptParams(AlwaysTrueScriptParams, size), alwaysSucceedsCompiled )
import Data.ByteString qualified as B
import Data.ByteString.Short qualified as BS
import Data.ByteString.Base16 as B16
import Data.ByteString.Short (fromShort)
import Numeric (showHex)

-- import Cardano.Api (writeFileTextEnvelope,  SerialiseAsCBOR (deserialiseFromCBOR), AsType (AsScript, AsPlutusScriptV2), Script, PlutusScriptVersion (PlutusScriptV2), PlutusScriptV2, Script)
-- import Plutus.V2.Ledger.Api (fromCompiledCode)
-- import Codec.Serialise (serialise)

scriptToCBOR = B.toStrict . serialise . fromCompiledCode


main =
  case deserialiseFromCBOR (AsScript AsPlutusScriptV2) $ scriptToCBOR alwaysSucceedsCompiled of
    Left err -> print err
    Right script ->
      writeFileTextEnvelope @(Script PlutusScriptV2) "always-succeeds.plutus" (Just "My script") script
      >>= either print return

-- main :: IO ()
-- main = do
--   print $ show $ fromShort ser
--   print $ show $ B16.encode $ fromShort ser


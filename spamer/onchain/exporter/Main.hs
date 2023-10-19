module Main where
import AlwaysTrueScript
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as BS
import PlutusLedgerApi.V2 qualified as V2

main :: IO ()
main = B.writeFile "validator.uplc" . Base16.encode $ BS.fromShort serialisedScript
  where
    script' = script params
    serialisedScript = V2.serialiseCompiledCode script'
    params =
     AlwaysTrueScriptParams { size = 1 }




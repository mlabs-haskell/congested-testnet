module Main where
import AlwaysTrueScript
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as BS
import PlutusLedgerApi.V2 qualified as V2

main :: IO ()
main = do 
  B.writeFile "validator.uplc" . Base16.encode $ serialisedScriptByteString
  print scriptLength
    where
      script' = script params
      serialisedScript = V2.serialiseCompiledCode script'
      serialisedScriptByteString = BS.fromShort serialisedScript
      scriptLength = B.length serialisedScriptByteString
      params =
       AlwaysTrueScriptParams { size = 200}




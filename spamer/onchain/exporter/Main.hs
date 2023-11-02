module Main where
import AlwaysTrueScript
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as BS
import PlutusLedgerApi.V2 qualified as V2
import System.Environment (getArgs)

main :: IO ()
main = do 
  -- [fpath size] <- getArgs
  let fpath = "validator.uplc"
  B.writeFile fpath . Base16.encode $ serialisedScriptByteString
  print scriptLength
    where
      script' = script params
      serialisedScript = V2.serialiseCompiledCode script'
      serialisedScriptByteString = BS.fromShort serialisedScript
      scriptLength = B.length serialisedScriptByteString
      params =
       AlwaysTrueScriptParams { size = 2}




module InsertScripts where

import Contract.Prelude

import Control.Monad.Error.Class (liftMaybe)
import Data.Array (filter, intersperse, (!!))
import Data.Array as Array
import Data.String.Utils (lines)
import Effect.Aff (error, launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Spammer.Db (executeQuery)
import Spammer.Query.Utils (bytea, decodeAikenHexToScriptHex)

main :: Effect Unit
main = do
  arr_scripts_fpath <- argv
  scripts_fpath <- liftMaybe (error "no script path") $ arr_scripts_fpath !! 1
  content <- readTextFile UTF8 scripts_fpath
  log $ show content
  let
    mlines = traverseDefault decodeAikenHexToScriptHex $ filter (\x -> x /= "") $ (lines content)
  lines' <- liftMaybe (error "decode error in aiken hex") mlines
  log $ show lines'

  let
    query = "INSERT INTO validators (validator, time) VALUES "
      <> (Array.fold <<< intersperse "," $ (\line -> "(" <> bytea line <> ",NOW())") <$> lines')
      <> "ON CONFLICT (validator) DO NOTHING;"
  launchAff_ $ executeQuery query


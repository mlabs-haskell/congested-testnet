module InsertScripts where

import Contract.Prelude

import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, (!!))
import Data.String.Utils (lines)
import Effect.Aff (error, launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (argv)
import Spammer.Db (executeQuery)
import Spammer.Utils (quotes)

main :: Effect Unit
main = do
  arr_scripts_fpath <- argv
  scripts_fpath <- liftMaybe (error "no script path") $ arr_scripts_fpath !! 1
  content <- readTextFile UTF8 scripts_fpath
  let

    bytea line = "decode( " <> quotes line <> ", 'hex' )"

    query = "INSERT INTO scripts (script, time) VALUES "
      <> foldl (\values line -> values <> " ( " <> bytea line <> " , NOW() ) ,") "" (lines content)
      <> "('' , NOW()) "
      <> "ON CONFLICT (script) DO NOTHING;"
  launchAff_ $ executeQuery query


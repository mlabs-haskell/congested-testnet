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
import Spammer.Query.Utils (bytea)

main :: Effect Unit
main = do
  arr_scripts_fpath <- argv
  scripts_fpath <- liftMaybe (error "no script path") $ arr_scripts_fpath !! 1
  content <- readTextFile UTF8 scripts_fpath
  let


    query = "INSERT INTO validators (validator, time) VALUES "
      <> foldl (\values line -> values <> " ( " <>  bytea line <> " , NOW() ) ,") "" (lines content)
      <> "('' , NOW()) "
      <> "ON CONFLICT (validator) DO NOTHING;"
  launchAff_ $ executeQuery query


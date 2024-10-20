{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "bignumber"
  , "cardano-transaction-lib"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "exceptions"
  , "http-methods"
  , "lists"
  , "maybe"
  , "mote"
  , "noble-secp256k1"
  , "node-buffer"
  , "node-fs"
  , "node-http"
  , "node-process"
  , "node-streams"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "safely"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "uint"
  ]
, packages = ./packages.dhall
-- , sources = [ "src/**/*.purs", "app/**/*.purs", "test/**/*.purs" ]
, sources = [ "app/Spammer.purs"]
}

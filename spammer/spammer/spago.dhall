{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "maybe"
  , "mote"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "spec"
  , "uint"
  , "noble-secp256k1"
  , "console"
  , "exceptions"
  , "transformers"
  , "node-buffer"
  , "node-fs"
  , "numbers"
  , "aff-promise"
  , "arrays"
  , "aeson"
  , "argonaut"
  , "strings"
  , "bignumber"
  , "node-process"
  , "stringutils"
  , "control"
  , "sequences"
  , "safely"
  , "lists"
  , "node-http"
  , "node-streams"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "app/**/*.purs", "test/**/*.purs"]
-- , sources = ["app/Faucet.purs", "src/Contracts/Faucet.purs","src/Keys.purs", "src/Config.purs"]
}

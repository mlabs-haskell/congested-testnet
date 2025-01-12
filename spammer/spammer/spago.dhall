{ name = "spammer"
, dependencies =
  [ "aeson"
  , "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "arrays"
  , "avar"
  , "bigints"
  , "bignumber"
  , "bytearrays"
  , "cardano-serialization-lib"
  , "cardano-transaction-builder"
  , "cardano-transaction-lib"
  , "cardano-types"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foreign"
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
  , "now"
  , "nullable"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "posix-types"
  , "prelude"
  , "random"
  , "refs"
  , "safely"
  , "spec"
  , "st"
  , "strings"
  , "stringutils"
  , "tailrec"
  , "transformers"
  , "typelevel"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/Worker.purs"]
}

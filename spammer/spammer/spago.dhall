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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}

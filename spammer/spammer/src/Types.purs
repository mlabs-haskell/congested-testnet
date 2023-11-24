module Spammer.Types where

-- | Typical key file format  
type KeyFile = { 
  type :: String,
  description :: String,
  cborHex :: String
  }

type CborHex = String

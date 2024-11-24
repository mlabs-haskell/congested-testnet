module Main where

import Contract.Prelude 

foreign import spammers :: Effect Unit


main :: Effect Unit
main = spammers

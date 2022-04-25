module Main where

import SignalGraph (parseNprintGraph)

main :: IO ()
main = getLine >>= parseNprintGraph


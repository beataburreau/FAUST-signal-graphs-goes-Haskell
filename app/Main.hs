module Main where

import SignalGraph (parseNprintGraph)
import Data.List (isSuffixOf)

main :: IO ()
main = do
        putStrLn "Enter relative file path to a .dot file:"
        path <- getLine
        if ".dot" `isSuffixOf` path
        then parseNprintGraph path
        else errorWithoutStackTrace "The provided file is not a .dot file"


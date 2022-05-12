{-# LANGUAGE LambdaCase #-}
module Main where

import SignalGraph (haskelliseFile)
import Data.List (isSuffixOf)
import GraphMonad (runGraphM, initState, State)
import GraphPrinter (prettyDetailedPrint)

-- Parses the content of given file into an inductive dynamic graph and prints it
main :: IO ()
main = do
        putStrLn "Enter relative file path to a .dot file:"
        path <- getLine
        if ".dot" `isSuffixOf` path
        then do
                res <- runGraphM initState (haskelliseFile path)
                case res of
                        Left err         -> putStrLn ("\nParse failed with error:\n" ++ show err)
                        Right (_, state) -> prettyDetailedPrint state >> putStrLn "Parse successful!"
        else errorWithoutStackTrace "The provided file is not a .dot file"

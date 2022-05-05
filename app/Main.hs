module Main where

import SignalGraph (haskelliseFile)
import Data.List (isSuffixOf)
import GraphMonad (runGraphM, initState)
import Data.Graph.Inductive (prettyPrint)

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
                        Right (graph, _) -> prettyPrint graph >> putStrLn "Parse successful!"
        else errorWithoutStackTrace "The provided file is not a .dot file"

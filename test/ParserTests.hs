module ParserTests (runTests) where
import Control.Monad.State (evalStateT)
import SignalGraph (haskelliseFile)
import GraphLabels (N, E)
import Data.Graph.Inductive (Gr, Graph (empty))
import GraphMonad (GraphM, runGraphM, initState)

runTests :: IO Bool 
runTests = do 
            res <- runGraphM initState $ mapM runTest tests
            case res of
                Left err      -> putStrLn ("Parser test failed with error: " ++ show err) >> return False
                Right correct -> putStrLn "Parser tests successful" >> return True

runTest :: String -> GraphM (Gr N E)
runTest fileName = haskelliseFile $ testPath fileName

testPath :: String -> String
testPath testName = "test/test-files/" ++ testName

tests :: [FilePath]
tests =
  [ 
  "+.dsp-sig.dot",
  "3.dsp-sig.dot", 
  "amp-hslider.dsp-sig.dot", 
  "compressor.dsp-sig.dot", 
  "div.dsp-sig.dot", 
  "hslider.dsp-sig.dot", 
  "rec+1.dsp-sig.dot",
  "weighted-sum.dsp-sig.dot"
  ]
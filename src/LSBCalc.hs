module LSBCalc (lsbI2O, lsbO2I) where


-- GRAPH STATE
import qualified GraphMonad as GM (Type (..), Node(..), Edge (..), SFix(..), Primitive(..), UI(..), Operator(..), Math(..), Error(..), cleanShow, lookupEdge, lookupNode)

-- LIBRARIES
import Data.List (sortBy, find)
import Control.Exception (throw)
import Data.Char (intToDigit)
import Numeric (floatToDigits, showIntAtBase)
import GHC.Float (int2Float)
import Control.Arrow (second)
import Data.Graph.Inductive as G (Node)
import Data.Maybe (fromJust)
import Sfix (inputLSB, maxLSB, outputLSB, recursionLSB)

-- From the inputs to the output
--        current node -> incoming edges  -> all nodes     -> Maybe lsb
lsbI2O :: GM.Node Int -> [GM.Edge Int]    -> [GM.Node Int] -> Maybe Int
lsbI2O n' ies ns = case GM.nprimitive n' of
                      GM.Number e      -> case e of
                                          Left  i         -> Just 0
                                          Right f         -> Just $ floatsLSB f
                      GM.Operator op   -> case op of
                                          GM.IntCast      -> Just 0
                                          GM.FloatCast    -> case GM.ntype n' of
                                                              GM.Integer -> Just 0
                                                              _          -> Just $ getLSB 0 
                                          GM.Cut          -> Nothing
                                          GM.Delay        -> case GM.nprimitive $ fromJust $ GM.lookupNode (GM.ei1 $ getArg 0) ns of
                                                                GM.Proj i   -> Just recursionLSB
                                                                GM.Rec i    -> Just recursionLSB
                                                                _           -> Just $ getLSB 0
                                          GM.Pow          -> undefined -- to be defined
                                          GM.Mul          -> Just $ getLSB 0 + getLSB 1
                                          GM.Div          -> Just $ getLSB 0 - getLSB 1 -- given that the division comes out 'even', otherwise rounding is applied
                                          GM.Mod          -> Just minimumLSB
                                          GM.Add          -> Just minimumLSB
                                          GM.Sub          -> Just minimumLSB
                                          GM.LT           -> Just 0
                                          GM.LE           -> Just 0
                                          GM.GT           -> Just 0
                                          GM.GE           -> Just 0
                                          GM.EQ           -> Just 0
                                          GM.NEQ          -> Just 0
                                          GM.XOR          -> Just minimumLSB
                                          GM.AND          -> Just minimumLSB
                                          GM.OR           -> Just minimumLSB
                                          GM.LShift       -> Just $ getLSB 0 - 1 
                                          GM.RShift       -> Just $ getLSB 0 + 1 
                      GM.Math math     -> case math of
                                          GM.Cos          -> Just minimumLSB 
                                          GM.MPow         -> Just minimumLSB 
                                          GM.Min          -> Just minimumLSB 
                                          GM.Max          -> Just minimumLSB 
                                          _               -> undefined -- to be defined
                      GM.UI ui         -> case ui of
                                          GM.Button       -> Just 0 
                                          GM.Checkbox     -> Just 0 
                                          GM.HSlider step -> Just $ floatsLSB step 
                                          GM.VSlider step -> Just $ floatsLSB step
                                          GM.Nentry step  -> Just $ floatsLSB step
                      GM.Proj i        -> Just recursionLSB
                      GM.Rec i         -> Just recursionLSB
                      GM.FSamplingFreq -> Just 0
                      GM.Input i       -> Just inputLSB
                      GM.Output        -> Nothing 
                      where
                          -- Gives the incoming edge with given arument no.
                          getArg :: Int -> GM.Edge Int
                          getArg argno = case find (\e -> GM.earg e == argno) ies of
                                          Just e  -> e
                                          Nothing -> throw $ GM.InferenceError $ "No incoming edge with arg. no. " ++ show argno
                          -- Gives the LSB of the edge with given argument no.
                          --        arg. no. -> lsb
                          getLSB :: Int      -> Int
                          getLSB argno = let edge = getArg argno in 
                                          case GM.lsb $ GM.esfix $ getArg argno of 
                                            Just lsb -> lsb
                                            Nothing  -> throw $ GM.InferenceError $ "No lsb set for edge " ++ GM.cleanShow edge
                          minimumLSB :: Int
                          minimumLSB = minimum $ map (fromJust . GM.lsb . GM.esfix) ies
                        

getEdge :: G.Node -> G.Node -> [GM.Edge Int] -> GM.Edge Int
getEdge n1 n2 es = case GM.lookupEdge n1 n2 es of
                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n1 ++ " and " ++ show n2
                        Just e  -> e

-- From the output to the inputs
--        current node -> incoming edges -> outgoing edges -> all nodes     -> Maybe [(arg. no., lsb)]
lsbO2I :: GM.Node Int  -> [GM.Edge Int]  -> [GM.Edge Int]  -> [GM.Node Int] -> Maybe [(Int, Int)]
lsbO2I n' ies oes ns = case GM.nprimitive n' of
                      GM.Output -> if outputLSB > head ilsbs
                                    then Just [(0, outputLSB)]
                                    else Nothing
                                    where
                                        args = argsAndLsbs ies
                                        ilsbs = map snd $ sortByFst args
                      _         -> case lsbI2O n' ies ns of
                                    Nothing     -> Nothing
                                    Just lsb    -> let diff = olsb - lsb in
                                                    if lsb >= olsb
                                                    then Nothing -- i.e. no lsb changes, no need to propagate further upwards in graph
                                                    --  diff > 0
                                                    else case GM.nprimitive n' of
                                                            GM.Number e      -> Nothing -- S^0 -> ...
                                                            GM.Operator op   -> case op of
                                                                                GM.IntCast      -> Just [(0, olsb)]
                                                                                GM.FloatCast    -> Just [(0, olsb)]
                                                                                GM.Cut          -> throw $ GM.InferenceError "LSB trimming not possible for cut, since the operation has no output"
                                                                                GM.Delay        -> Just [(0, olsb), ilsbs !! 1]
                                                                                GM.Pow          -> undefined -- ??
                                                                                GM.Mul          -> Just $ increaseLSBsEvenly diff ilsbs
                                                                                GM.Div          -> Just $ second (+ diff) (head ilsbs) : tail ilsbs
                                                                                GM.Mod          -> Just $ setSmallerLSBsto olsb ilsbs
                                                                                GM.Add          -> Just $ setSmallerLSBsto olsb ilsbs
                                                                                GM.Sub          -> Just $ setSmallerLSBsto olsb ilsbs
                                                                                GM.LT           -> Nothing -- the output precision of any boolean operator is 0 which shouldn't be propagated, since we still want to keep the comparisons as exact as possible
                                                                                GM.LE           -> Nothing
                                                                                GM.GT           -> Nothing
                                                                                GM.GE           -> Nothing
                                                                                GM.EQ           -> Nothing
                                                                                GM.NEQ          -> Nothing
                                                                                GM.XOR          -> Just $ setSmallerLSBsto olsb ilsbs
                                                                                GM.AND          -> Just $ setSmallerLSBsto olsb ilsbs
                                                                                GM.OR           -> Just $ setSmallerLSBsto olsb ilsbs
                                                                                GM.LShift       -> Just [(0, olsb + 1)]
                                                                                GM.RShift       -> Just [(0, olsb - 1)]
                                                            GM.Math math     -> undefined -- to be defined
                                                            GM.UI ui         -> Nothing -- S^0 -> ...
                                                            GM.Proj i        -> Nothing
                                                            GM.FSamplingFreq -> Just [(0, olsb)]
                                                            GM.Rec i         -> Nothing 
                                                            GM.Input i       -> Nothing -- S^0 -> ...
                                    where
                                        args = argsAndLsbs ies
                                        ilsbs = sortByFst args
                                        olsb  = snd $ head $ sortByFst $ argsAndLsbs oes
                                        -- any lsb less than limit is set to limit
                                        setSmallerLSBsto :: Int -> [(Int, Int)] -> [(Int, Int)]
                                        setSmallerLSBsto limit = map (second toLimit)
                                                              where
                                                                toLimit :: Int -> Int
                                                                toLimit lsb = if lsb < limit
                                                                              then limit
                                                                              else lsb
                                        -- distributes diff evently between the lsbs
                                        increaseLSBsEvenly :: Int -> [(Int, Int)] -> [(Int, Int)]
                                        increaseLSBsEvenly 0    lsbs = lsbs
                                        increaseLSBsEvenly diff lsbs = increaseLSBsEvenly (diff - 1) $ tail lsbs ++ [second (+1) $ head lsbs]


argsAndLsbs :: [GM.Edge Int] -> [(Int, Int)]
argsAndLsbs = map (\e -> case GM.lsb $ GM.esfix e of
                            Nothing  -> throw $ GM.InferenceError $ "lsb missing for edge " ++ GM.cleanShow e
                            Just lsb -> (GM.earg e, lsb))

-- Sorts supplied list of tuples based on the first element in each tuple
sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\(a,_) (b,_) -> compare a b)

floatsLSB :: Float -> Int
floatsLSB f = negate . length . takeWhile (/= 'p') . tail . dropWhile (/= '.') . take (negate maxLSB) $ dec2bin $ f - (int2Float . floor) f

dec2bin :: RealFloat a => a -> String
dec2bin f = "0." ++ map intToDigit digits ++ "p+" ++ showIntAtBase 2 intToDigit ex ""
  where (digits, ex) = floatToDigits 2 f
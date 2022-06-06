module LSBInference (inferLSBs, trimLSBs) where

-- GRAPH STATE
import qualified GraphMonad as GM (GraphM, State, Type (..), Node(..), Primitive(..), UI(..), Operator(..), Edge (..), SFix(..), Error(..), lookupNode, lookupEdge)

-- GRAPH LABELS
import GraphLabels (N, E)

-- PRECISIONS 
import Precisions (inputLSB, maxLSB, outputLSB)

-- LIBRARIES
import qualified Data.Graph.Inductive as G
    ( Gr,
      Context,
      gmap,
      ufold,
      Graph,
      Graph(..),
      DynGraph(..),
      Node,
      pre,
      suc)
import Data.List (partition, sortBy, length, (\\))
import Control.Monad.State.Class (get, put)
import Data.Maybe (isJust, fromJust, catMaybes, isNothing)
import Control.Monad (foldM, liftM)
import Control.Exception (throw, try)
import Data.Char (intToDigit)
import Numeric (floatToDigits, showIntAtBase)
import GHC.Float (int2Float)

import Control.Arrow (second)
import GraphMonad (Primitive(Output))


inferLSBs :: G.Gr N E -> G.Node -> GM.GraphM (G.Gr N E)
inferLSBs gr n = do
                (ns, es) <- get

                -- ies:  incoming edges to node n
                let ies = map (\n' -> case GM.lookupEdge n' n es of
                                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n' ++ " and " ++ show n
                                        Just e  -> e) (G.pre gr n)
                let iesWOlsb = filter (isNothing . GM.lsb . GM.esfix) ies

                -- oes:  outgoing edges from node n
                let oes = map (\n' -> case GM.lookupEdge n n' es of
                                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n ++ " and " ++ show n'
                                        Just e  -> e) (G.suc gr n)

                if not $ null iesWOlsb

                -- some incoming edges lack LSBs, backtrack and infer LSBs for those missing it
                then inferLSBs gr $ GM.ei1 $ head iesWOlsb

                -- all incoming edges have LSBs, infer LSBs for outgoing edges
                else if not $ null oes

                     then case GM.lookupNode n ns of
                            Nothing -> throw $ GM.GraphError "Couldn't find current node in the graph state's node list"
                            Just n'  -> let args = argsAndLsbs ies
                                        in case lsbI2O n' args of
                                                Nothing     -> return ()
                                                Just lsb    -> put (ns, (es \\ oes) ++ map (setLSB lsb) oes)
                                        -- fold inferLSBs over the nodes that the outgoing edges are pointing to
                                        >> foldM inferLSBs gr (map GM.ei2 oes)

                     -- no outgoing edges -> done
                     else return gr


-- From the inputs to the output
--        current node -> [(arg. no., lsb)] -> Maybe lsb
lsbI2O :: GM.Node Int  -> [(Int, Int)]      -> Maybe Int
lsbI2O n' args = case GM.nprimitive n' of
                GM.Number e     -> case e of
                                    Left  i         -> Just 0
                                    Right f         -> Just $ floatsLSB f
                GM.Operator op  -> case op of
                                    GM.IntCast      -> Just 0
                                    GM.FloatCast    -> case GM.ntype n' of
                                                        GM.Integer -> Just 0
                                                        _          -> Just $ head lsbs
                                    GM.Cut          -> Nothing
                                    GM.Delay        -> Just $ head lsbs
                                    GM.Pow          -> undefined -- lsb of arg 0 * VALUE of arg 1 (dubbelkolla vilket argument som är vilket) (value -> interval??)
                                    GM.Mul          -> Just $ sum $ take 2 lsbs
                                    GM.Div          -> Just $ lsbs !! 0 - lsbs !! 1 -- given that the division comes out 'even', otherwise rounding is applied
                                    GM.Mod          -> Just $ minimum lsbs
                                    GM.Add          -> Just $ minimum lsbs
                                    GM.Sub          -> Just $ minimum lsbs
                                    GM.LT           -> Just 0
                                    GM.LE           -> Just 0
                                    GM.GT           -> Just 0
                                    GM.GE           -> Just 0
                                    GM.EQ           -> Just 0
                                    GM.NEQ          -> Just 0
                                    GM.XOR          -> Just $ minimum lsbs
                                    GM.AND          -> Just $ minimum lsbs
                                    GM.OR           -> Just $ minimum lsbs
                                    GM.LShift       -> Just $ head lsbs - 1
                                    GM.RShift       -> Just $ head lsbs + 1
                GM.UI ui        -> case ui of
                                    GM.Button       -> Just 0 -- bc outputs 0 or 1 right? test this and check type (should be Integer) 
                                    GM.Checkbox     -> Just 0 -- bc outputs 0 or 1 right? test this and check type (should be Integer) 
                                    GM.HSlider step -> Just $ floatsLSB step -- OBS inte säkert att man bara kan basera LSB:n på step, min(step, init)
                                    GM.VSlider step -> Just $ floatsLSB step
                                    GM.Nentry step  -> Just $ floatsLSB step
                GM.Proj i       -> throw $ GM.InferenceError "LSB inference not yet implemented for recursive signals"
                GM.Rec i        -> throw $ GM.InferenceError "LSB inference not yet implemented for recursive signals"
                GM.Input i      -> Just inputLSB
                GM.Output       -> Nothing -- Just $ head lsbs
                where
                    -- LSBs in argument order
                    lsbs :: [Int]
                    lsbs = map snd $ sortByFst args


trimLSBs :: G.Gr N E -> G.Node -> GM.GraphM (G.Gr N E)
trimLSBs gr n = do
                (ns, es) <- get

                -- ies:  incoming edges to node n
                let ies = map (\n' -> case GM.lookupEdge n' n es of
                                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n' ++ " and " ++ show n
                                        Just e  -> e) (G.pre gr n)

                -- oes:  outgoing edges from node n
                let oes = map (\n' -> case GM.lookupEdge n n' es of
                                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n ++ " and " ++ show n'
                                        Just e  -> e) (G.suc gr n)

                if all (isJust . GM.lsb . GM.esfix) es

                -- all outgoing edges have LSBs, trim LSB for the incoming edges
                then if not $ null ies

                     then case GM.lookupNode n ns of
                         Nothing -> throw $ GM.GraphError "Couldn't find current node in the graph state's node list"
                         Just n' -> let oargs = argsAndLsbs oes
                                        iargs = argsAndLsbs ies
                                    in case lsbO2I n' iargs oargs of
                                        Nothing   -> return ()
                                        Just lsbs -> put (ns, (es \\ ies) ++ map (\e -> setLSB (fromJust $ lookup (GM.earg e) lsbs) e) ies)
                                     -- fold trimLSBs over the nodes that the incoming edges are pointing to
                                    >> foldM trimLSBs gr (map GM.ei1 ies)
                     -- no incoming edges -> done
                     else return gr

                else throw $ GM.InferenceError $ "Cannot trim LSBs of incoming edges to node " ++ show n ++ " because the fix-point format is not defined for all edges in the graph"


-- From the output to the inputs
--        current node -> [(arg. no., lsb)] -> [(output no., lsb)] -> Maybe [(arg. no., lsb)]
lsbO2I :: GM.Node Int  -> [(Int, Int)]      -> [(Int, Int)]        -> Maybe [(Int, Int)]
lsbO2I n' args os = case GM.nprimitive n' of
                      GM.Output -> if outputLSB > head ilsbs
                                    then Just [(0, outputLSB)]
                                    else Nothing
                                    where
                                        ilsbs = map snd $ sortByFst args
                      _         -> case lsbI2O n' args of
                                    Nothing     -> Nothing
                                    Just lsb    -> let diff = olsb - lsb in
                                                    if lsb >= olsb
                                                    then Nothing -- i.e. no lsb changes, no need to propagate further upwards in graph
                                                    --  diff > 0
                                                    else case GM.nprimitive n' of
                                                            GM.Number e     -> Nothing -- S^0 -> ...
                                                            GM.Operator op  -> case op of
                                                                                GM.IntCast      -> Just [(0, olsb)]
                                                                                GM.FloatCast    -> Just [(0, olsb)]
                                                                                GM.Cut          -> throw $ GM.InferenceError "LSB trimming not possible for cut, since the operation has no output"
                                                                                GM.Delay        -> Just [(0, olsb)]
                                                                                GM.Pow          -> undefined -- ???
                                                                                GM.Mul          -> Just $ map (second (+ diff `div` 2)) ilsbs -- OBS vid udda diff går 1 lsb 'förlorad'
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
                                                            GM.UI ui        -> Nothing -- S^0 -> ...
                                                            GM.Proj i       -> throw $ GM.InferenceError "LSB trimming not yet implemented for recursive signals"
                                                            GM.Rec i        -> throw $ GM.InferenceError "LSB trimming not yet implemented for recursive signals"
                                                            GM.Input i      -> Nothing -- S^0 -> ...
                                    where
                                        ilsbs = sortByFst args
                                        olsb  = snd $ head $ sortByFst os
                                        -- any lsb less than limit is set to limit
                                        setSmallerLSBsto :: Int -> [(Int, Int)] -> [(Int, Int)]
                                        setSmallerLSBsto limit = map (second toLimit)
                                                              where
                                                                toLimit :: Int -> Int
                                                                toLimit lsb = if lsb < limit
                                                                              then limit
                                                                              else lsb


argsAndLsbs :: [GM.Edge Int] -> [(Int, Int)]
argsAndLsbs = map (\e -> (GM.earg e, fromJust $ GM.lsb $ GM.esfix e))

setLSB :: Int -> GM.Edge Int -> GM.Edge Int
setLSB lsb e@(GM.Edge _ _ _ _ _ _ (GM.SFix msb _)) = e {GM.esfix = GM.SFix msb (Just lsb)}

-- Sorts supplied list of tuples based on the first element in each tuple
sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\(a,_) (b,_) -> compare a b)

floatsLSB :: Float -> Int
floatsLSB f = negate . length . takeWhile (/= 'p') . tail . dropWhile (/= '.') . take (negate maxLSB) $ dec2bin $ f - (int2Float . floor) f

dec2bin :: RealFloat a => a -> String
dec2bin f = "0." ++ map intToDigit digits ++ "p+" ++ showIntAtBase 2 intToDigit ex ""
  where (digits, ex) = floatToDigits 2 f

-- For debugging purposes
cleanShow :: [GM.Edge Int] -> String
cleanShow []     = ""
cleanShow (e:es) = cleanShow es ++ "\n" ++ show (GM.ei1 e) ++ " -> " ++ show (GM.ei2 e)
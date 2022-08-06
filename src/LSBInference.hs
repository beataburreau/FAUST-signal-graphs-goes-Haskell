module LSBInference (inferLSBs) where


-- GRAPH STATE
import qualified GraphMonad as GM
      (GraphM,
      Node(..),
      Edge (..),
      SFix(..),
      Error(..),
      lookupNode,
      lookupEdge,
      addPathSucc,
      setLSB,
      setPathSuccs,
      removePathSucc,
      cleanShow)

-- GRAPH LABELS
import GraphLabels (N, E)

-- LSB CALC
import LSBCalc (lsbI2O, lsbI2O)

-- LIBRARIES
import qualified Data.Graph.Inductive as G (Gr, Node, pre, suc)

import Data.List ((\\), nub)
import Control.Monad.State.Class (get, put)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Control.Monad (foldM)
import Control.Exception (throw)

inferLSBs :: G.Gr N E -> G.Node -> GM.GraphM (G.Gr N E)
inferLSBs gr n = do
                (ns, es) <- get

                -- all predecessors of n
                let grPreds = nub $ G.pre gr n

                -- n's node information
                let n' = case GM.lookupNode n ns of
                            Nothing -> throw $ GM.GraphError "Couldn't find current node in the graph state's node list"
                            Just n' -> n'

                -- predecessors of n for which the edges (predecessor -> n) have not yet been traversed
                let grPreds' = filter (notElem n . GM.npathsuccs) $ mapMaybe (`GM.lookupNode` ns) grPreds

                -- ies:  incoming edges to node n
                let ies = map (\n' -> case GM.lookupEdge n' n es of
                                         Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n' ++ " and " ++ show n
                                         Just e  -> e) grPreds

                if not $ null grPreds'
                -- add n as path successor for pred, first node in graphPreds' to indicate that the edge (pred -> n) has been traversed
                -- then call inferLSBs for pred
                then let pred = head grPreds'
                      in put (GM.addPathSucc n pred : (ns \\ [pred]), es)
                      >> inferLSBs gr (GM.ni pred)

                -- all edges (pred -> n) have been traversed (if any)
                -- remove any successive nodes that already have LSBs set, then calculate and set LSB for the successive node next in turn
                else let n'' = dropSuccsWlsb es n'
                         succs = GM.npathsuccs n''
                      in if not $ null succs
                          -- infer LSB for the edge (n -> succ)
                          then
                            let succ = head succs
                                e = getEdge n succ es
                                  in case lsbI2O n'' ies ns of 
                                  Nothing     -> return ()
                                  Just lsb    -> put (n'' : (ns \\ [n']), GM.setLSB lsb e : (es \\ [e]))

                                >> inferLSBs gr succ
                          -- no successors on path -> done
                          -- return graph with cleared pathSuccs 
                          else put (map (GM.setPathSuccs []) ns, es) >> return gr
                  where
                    -- removes nodes from n's successor list for which the edge (n' -> succ) has LSB set
                    dropSuccsWlsb :: [GM.Edge Int] -> GM.Node Int -> GM.Node Int
                    dropSuccsWlsb _  n'@(GM.Node _ _ _ _ _ [])    = n'
                    dropSuccsWlsb es n'@(GM.Node _ _ _ _ _ succs) = case GM.lookupEdge (GM.ni n') (head succs) es of
                                            Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show (GM.ni n') ++ " and " ++ show (head succs)
                                            Just e  -> if isJust $ GM.lsb $ GM.esfix e
                                                      then dropSuccsWlsb es $ GM.removePathSucc n'
                                                      else n'

getEdge :: G.Node -> G.Node -> [GM.Edge Int] -> GM.Edge Int
getEdge n1 n2 es = case GM.lookupEdge n1 n2 es of
                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n1 ++ " and " ++ show n2
                        Just e  -> e

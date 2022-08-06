module LSBTrimming (trimLSBs) where 


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
      getLastPathSucc, 
      setLSB, 
      setPathSuccs)

-- GRAPH LABELS
import GraphLabels (N, E)

-- LSB CALC
import LSBCalc (lsbO2I)

-- LIBRARIES
import qualified Data.Graph.Inductive as G (Gr, Node, pre, suc)

import Data.List ((\\), nub)
import Control.Monad.State.Class (get, put)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Control.Monad (foldM)
import Control.Exception (throw)

-- Precondition: all edges in graph have LSBs set
trimLSBs :: G.Gr N E -> G.Node -> GM.GraphM (G.Gr N E)
trimLSBs gr n = do
                (ns, es) <- get

                -- all predecessors of n
                let grPreds = nub $ G.pre gr n

                let n' = case GM.lookupNode n ns of
                          Nothing -> throw $ GM.GraphError "Couldn't find current node in the graph state's node list"
                          Just n' -> n'

                -- predecessors of n for which the edges (predecessor -> n) have not yet been traversed
                let grPreds' = filter (notElem n . GM.npathsuccs) $ mapMaybe (`GM.lookupNode` ns) grPreds

                -- ies:  incoming edges to node n
                let ies = map (\n' -> case GM.lookupEdge n' n es of
                                         Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n' ++ " and " ++ show n
                                         Just e  -> e) grPreds

                -- oes:  outgoing edges from node n
                let oes = map (\n' -> case GM.lookupEdge n n' es of
                                        Nothing -> throw $ GM.GraphError $ "Missing additional information for edge between nodes " ++ show n ++ " and " ++ show n'
                                        Just e  -> e) (G.suc gr n)

                -- trim LSB for the untraversed incoming edges
                if not $ null grPreds'

                then  let pred = head grPreds' in
                      if not $ all (GM.trimmed . GM.esfix) ies
                      -- trim LSBs of the incomming edges
                      then case lsbO2I n' ies oes ns of
                                  -- no LSB change for incoming edges, return to previous node on path (last successor)
                                  Nothing   -> case GM.getLastPathSucc n' of
                                                Just succ -> trimLSBs gr succ
                                                Nothing   -> put (map (GM.setPathSuccs []) ns, es) >> return gr -- return ()
                                  -- update LSBs for all incoming edges, add n as successor for the predecessor node next in line and call trimLSBs for said node
                                  Just lsbs -> put ((ns \\ [pred]) ++ [GM.addPathSucc n pred], (es \\ ies) ++ map (\e -> GM.setLSB (fromJust $ lookup (GM.earg e) lsbs) e) ies)
                                                  >> trimLSBs gr (GM.ni pred) 
                      -- LSBs of all incoming edges have already been trimmed, but there are still unvisited predecessors
                      -- call trimLSBs for the predecessor next in line
                      else put ((ns \\ [pred]) ++ [GM.addPathSucc n pred], es) >> trimLSBs gr (GM.ni pred)

                -- all incoming edges have been traversed
                else case GM.getLastPathSucc n' of
                      Just succ -> trimLSBs gr succ
                      -- no untraversed incoming edges, no outgoing edges -> done
                      -- return graph with cleared pathSuccs 
                      Nothing   -> put (map (GM.setPathSuccs []) ns, es) >> return gr

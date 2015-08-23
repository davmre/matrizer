module Matrizer.Search (
 SearchNode,
 astar,
 heuristicCost,
 nvalue
) where

import qualified Data.Heap as Heap
import qualified Data.Set as Set
import Data.Ix
import qualified Data.Sequence as Seq
import Debug.Trace

data SearchNode n c  = SearchNode {nvalue :: n, gcost :: c, nprev :: Maybe (SearchNode n c), move :: Maybe String} deriving (Eq, Show, Ord)

pprint::  (Ord n, Real c, Show c, Show n) => ( SearchNode n c -> c ) -> SearchNode n c -> String
pprint heuristic sn = let gc = gcost sn
                          hc = heuristic sn
                          fc = hc + gc in
                          "g " ++ (show gc) ++ ", h " ++ (show hc) ++ ", f " ++ (show fc) ++ ": " ++ (show $ nvalue sn) ++ "\n"
         

-- n: type representing an element of the search domain 
-- c: numeric type for the cost/heuristic functions
-- start: start state
-- succ: function that maps a state to a list of successor states with costs
-- goalCheck: test whether a state is a goal
-- cost: fringe ranking fn f(x), given current state x and cost-so-far g(x). for valid A*, this should return f(x) = g(x) + h(x) for some heuristic h(x). For best-first search, just return g(x); for greedy search, just return h(x). 
-- returns: goal state found, or Nothing if search completes without success.
astar :: (Ord n, Real c, Show c, Show n) => n -> (n -> [(n,c, Maybe String)]) -> (n -> Bool) -> ( SearchNode n c -> c ) -> c -> Maybe (SearchNode n c)
astar start succ goalCheck cost maxCost = astarIter (Heap.singleton (0, SearchNode {nvalue=start, gcost=0, nprev=Nothing, move=Nothing})) Set.empty succ goalCheck cost maxCost

-- how to structure a*? there is an iterative helper: given the fringe, we pop off an element, run the goal check, compute its successors, check closed set, add to fringe. 
astarIter :: (Ord n, Real c, Show c, Show n) => Heap.MinPrioHeap c (SearchNode n c) -> Set.Set n -> (n -> [(n, c, Maybe String)]) -> (n -> Bool) -> (SearchNode n c -> c) -> c -> Maybe (SearchNode n c)
astarIter fringe closed succ goalCheck cost maxCost = 
          do ((fcost, current ), diminishedFringe) <- Heap.view fringe
             --if goalCheck (nvalue (trace (pprint cost current) current)) then return current
             if goalCheck (nvalue  current) then return current
             else if (gcost current) > maxCost then Nothing
             else let newClosed = Set.insert (nvalue current) closed
                      successors = [ SearchNode {nvalue=s, gcost=(gcost current) + moveCost, nprev=Just current, move=smove} | (s, moveCost, smove) <- (succ (nvalue current)), Set.notMember s closed ]
                      succCosts = [ (cost sn, sn) | sn <- successors ]
                      newFringe = Heap.union diminishedFringe (Heap.fromList succCosts) in
                      astarIter newFringe newClosed succ goalCheck cost maxCost
          

-- convert a heuristic function into an AStar cost function
heuristicCost h node = (gcost node) + (h (nvalue node))


-- toy search example: n-queens
-- astar (queensDiag 8) queensSucc queensGoalCheck (heuristicCost queensHeuristic)
type QueenPos =  [(Int, Int)]
showQP pos = let n = length pos in
                 [if (p,q) `elem` pos then 'Q' else if q==n then '\n' else '*' | p <- range(0, n-1), q <- range(0, n)]

queensDiag n = [(i,i) | i <- range(0, n-1)]

attacks (rowA, colA) (rowB, colB) = (rowA==rowB) || (colA==colB) || (rowA-colA == rowB-colB) || (rowA+colA == rowB+colB)
allpairs pos = [(p1, p2) | p1 <- pos, p2 <- pos, p1 < p2]
queensHeuristic pos = sum [ fromEnum (attacks queen1 queen2) | (queen1, queen2) <- allpairs pos ]
queensGoalCheck pos = (queensHeuristic pos) == 0

queensSucc pos = [(s, 1, Nothing) | s <- queenSucc [] [] pos]
queenSucc :: [QueenPos] -> QueenPos -> QueenPos -> [QueenPos]
queenSucc s _ [] = s
queenSucc s prevQ ((row, col):nextQ) = let otherQ = prevQ ++ nextQ
                                           n = 1 + length otherQ 
                                           thisRow = [c | (r,c) <- otherQ , r==row]
                                           rowMinBound = maximum [c+1 | c <- -1:thisRow, c < col]
                                           rowMaxBound = minimum [c-1 | c <- n:thisRow, c > col]
                                           newRowPos = [prevQ ++ [(row, c)] ++ nextQ | c <- range (rowMinBound, rowMaxBound), c /= col] 
                                           thisCol = [r | (r,c) <- otherQ , c==col]
                                           colMinBound = maximum [r+1 | r <- -1:thisCol, r < row]
                                           colMaxBound = minimum [r-1 | r <- n:thisCol, r > row]
                                           newColPos = [prevQ ++ [(r, col)] ++ nextQ | r <- range (colMinBound, colMaxBound), r /= row]
                                           thisDiag1 = [r | (r,c) <- otherQ , r+c==row+col]
                                           d1MinBound = maximum [r+1 | r <- -1:thisDiag1, r < row]
                                           d1MaxBound = minimum [r-1 | r <- n:thisDiag1, r > row]
                                           newD1Pos = [prevQ ++ [(r, row+col-r)]  ++ nextQ | r <- range (d1MinBound, d1MaxBound), r /= row, row+col-r >= 0, row+col-r < n]
                                           thisDiag2 = [r | (r,c) <- otherQ , r-c==row-col]
                                           d2MinBound = maximum [r+1 | r <- -1:thisDiag2, r < row]
                                           d2MaxBound = minimum [r-1 | r <- n:thisDiag2, r > row]
                                           newD2Pos = [prevQ ++ [(r, r-(row-col))]  ++ nextQ | r <- range (d2MinBound, d2MaxBound), r /= row, r-(row-col) >= 0, r-(row-col) < n] in
                                          queenSucc (s ++ newRowPos ++ newColPos ++ newD1Pos ++ newD2Pos) (prevQ ++ [(row, col)]) nextQ


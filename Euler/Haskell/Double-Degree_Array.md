# Euler Problem: Double-Degree Array in Haskell

## Problem Statement
Given an undirected graph with n nodes and m edges, compute the double-degree of each node. The double-degree of a node is the sum of the degrees of all its neighbors.

## Solution Approach
1. Parse input to get the number of nodes and edges
2. Build an adjacency list representation of the graph
3. Calculate the degree of each node
4. For each node, sum the degrees of its neighbors to get the double-degree

## Haskell Implementation

```haskell
import Data.List (group, sort)
import qualified Data.Map as Map

-- Parse input lines into edges
parseEdges :: [String] -> [(Int, Int)]
parseEdges = map parseEdge
  where
    parseEdge line = let [u, v] = map read (words line) :: [Int] in (u, v)

-- Build adjacency list from edges
buildAdjacencyList :: [(Int, Int)] -> Map.Map Int [Int]
buildAdjacencyList edges = 
    let undirectedEdges = edges ++ map (\(u, v) -> (v, u)) edges
        sortedEdges = sort undirectedEdges
        groupedEdges = groupBy (\(u, _) (v, _) -> u == v) sortedEdges
    in Map.fromListWith (++) $ map (\es -> (fst (head es), map snd es)) groupedEdges
  where
    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy _ [] = []
    groupBy eq (x:xs) = let (ys, zs) = span (eq x) xs in (x:ys) : groupBy eq zs

-- Calculate degrees of all nodes
calculateDegrees :: Map.Map Int [Int] -> Map.Map Int Int
calculateDegrees adjList = Map.map length adjList

-- Calculate double-degree for each node
calculateDoubleDegrees :: Map.Map Int [Int] -> Map.Map Int Int
calculateDoubleDegrees adjList = 
    let degrees = calculateDegrees adjList
    in Map.mapWithKey (\node neighbors -> sum $ map (degrees Map.! ) neighbors) adjList

-- Main solution function
doubleDegreeArray :: [String] -> [Int]
doubleDegreeArray inputLines = 
    let [n, m] = map read (words (head inputLines)) :: [Int]
        edges = parseEdges (tail inputLines)
        adjList = buildAdjacencyList edges
        doubleDegrees = calculateDoubleDegrees adjList
    in map (\i -> doubleDegrees Map.! i) [1..n]

-- Alternative cleaner implementation using standard adjacency list
doubleDegreeArrayClean :: [String] -> [Int]
doubleDegreeArrayClean inputLines = 
    let [n, _] = map read (words (head inputLines)) :: [Int]
        edges = map (map read . words) (tail inputLines) :: [[Int]]
        -- Build adjacency list
        adjList = foldl addEdge Map.empty edges
        -- Calculate degrees
        degrees = Map.map length adjList
        -- Calculate double degrees
        doubleDegrees = map (\node -> 
            sum $ map (\neighbor -> degrees Map.! neighbor) (adjList Map.! node)
            ) [1..n]
    in doubleDegrees
  where
    addEdge acc (u, v) = 
        let newAcc = Map.insertWith (++) u [v] acc
            finalAcc = Map.insertWith (++) v [u] newAcc
        in finalAcc

-- Most concise implementation
doubleDegreeArrayFinal :: [String] -> [Int]
doubleDegreeArrayFinal inputLines = 
    let [n, _] = map read (words (head inputLines)) :: [Int]
        edges = map (map read . words) (tail inputLines) :: [[Int]]
        -- Create adjacency list
        adjList = Map.fromListWith (++) 
            [(u, [v]) | (u, v) <- edges] 
            `Map.unionWith` Map.fromListWith (++) 
            [(v, [u]) | (u, v) <- edges]
        -- Calculate degrees
        degrees = Map.map length adjList
        -- Calculate double degrees
        doubleDegs = [sum [degrees Map.! v | v <- adjList Map.! i] | i <- [1..n]]
    in doubleDegs

-- Example usage
main :: IO ()
main = do
    let input = ["5 4", "1 2", "2 3", "3 4", "4 5"]
    print $ doubleDegreeArrayFinal input
```

## Example Walkthrough

For input:
```
5 4
1 2
2 3
3 4
4 5
```

1. **Graph structure**: 
   - Node 1 connects to node 2
   - Node 2 connects to nodes 1 and 3
   - Node 3 connects to nodes 2 and 4
   - Node 4 connects to nodes 3 and 5
   - Node 5 connects to node 4

2. **Degrees**: 
   - Node 1: degree = 1
   - Node 2: degree = 2
   - Node 3: degree = 2
   - Node 4: degree = 2
   - Node 5: degree = 1

3. **Double-degrees**:
   - Node 1: neighbors = [2], double-degree = degree[2] = 2
   - Node 2: neighbors = [1,3], double-degree = degree[1] + degree[3] = 1 + 2 = 3
   - Node 3: neighbors = [2,4], double-degree = degree[2] + degree[4] = 2 + 2 = 4
   - Node 4: neighbors = [3,5], double-degree = degree[3] + degree[5] = 2 + 1 = 3
   - Node 5: neighbors = [4], double-degree = degree[4] = 2

## Output
```
[2,3,4,3,2]
```

## Time Complexity
- **Time**: O(n + m) where n is number of nodes and m is number of edges
- **Space**: O(n + m) for storing adjacency list and degree information

This solution efficiently handles the double-degree array computation using Haskell's functional programming features and immutable data structures.


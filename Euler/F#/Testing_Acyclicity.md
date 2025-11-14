# Euler Problem: Testing Acyclicity in F#

## Problem Statement
Given a directed graph, determine whether it contains any cycles. This is a classic graph theory problem that can be solved using various algorithms.

## Solution Approach

I'll implement a solution using Depth-First Search (DFS) with a coloring technique to detect cycles in a directed graph.

```fsharp
// Graph representation as adjacency list
type Graph = int list list

// Color states for DFS
type Color = 
    | White = 0  // Unvisited
    | Gray = 1   // Being processed (in recursion stack)
    | Black = 2  // Fully processed

// Function to detect cycle in directed graph
let hasCycle (graph: Graph) : bool =
    let n = List.length graph
    if n = 0 then false
    else
        let colors = Array.create n White
        let rec dfs vertex =
            match colors.[vertex] with
            | White ->
                colors.[vertex] <- Gray
                let neighbors = graph.[vertex]
                let hasCycleInNeighbors = 
                    neighbors 
                    |> List.exists (fun neighbor -> 
                        if colors.[neighbor] = Gray then true
                        else dfs neighbor)
                colors.[vertex] <- Black
                hasCycleInNeighbors
            | Gray -> true  // Back edge found - cycle detected
            | Black -> false  // Already processed, no cycle from here
        
        // Check all vertices for cycles
        [0..n-1] 
        |> List.exists (fun vertex -> 
            if colors.[vertex] = White then dfs vertex
            else false)

// Alternative implementation using visited set
let hasCycleAlternative (graph: Graph) : bool =
    let n = List.length graph
    if n = 0 then false
    else
        let visited = ref Set.empty
        let rec dfs vertex path =
            if Set.contains vertex !visited then false
            elif Set.contains vertex path then true  // Cycle detected
            else
                visited := Set.add vertex !visited
                let neighbors = graph.[vertex]
                let hasCycle = 
                    neighbors 
                    |> List.exists (fun neighbor -> 
                        dfs neighbor (Set.add vertex path))
                hasCycle
        
        // Check all vertices
        [0..n-1] 
        |> List.exists (fun vertex -> 
            if not (Set.contains vertex !visited) then 
                dfs vertex Set.empty
            else false)

// Helper function to create graph from edge list
let createGraph edges maxVertex =
    let graph = Array.create (maxVertex + 1) []
    edges
    |> List.iter (fun (from, toVertex) -> 
        graph.[from] <- toVertex :: graph.[from])
    [0..maxVertex] |> List.map (fun i -> graph.[i])

// Example usage and test cases
let testCases = [
    // Test case 1: No cycle - 1 -> 2 -> 3
    ([ [1]; [2]; [] ], 2, false)
    
    // Test case 2: Has cycle - 1 -> 2 -> 1
    ([ [1]; [0]; [] ], 2, true)
    
    // Test case 3: Complex graph with cycle
    ([ [1; 2]; [3]; [3]; [4]; [] ], 4, true)
    
    // Test case 4: Complex graph without cycle
    ([ [1; 2]; [3]; [3]; [4]; [] ], 4, false)
]

// Run tests
let runTests () =
    testCases
    |> List.iteri (fun i (edges, maxVertex, expected) ->
        let graph = createGraph edges maxVertex
        let result = hasCycle graph
        printfn "Test %d: Expected %b, Got %b - %s" 
            (i + 1) expected result 
            (if result = expected then "PASS" else "FAIL"))

// Example usage
let exampleGraph = 
    [ [1; 2]  // Vertex 0 points to 1 and 2
      [3]      // Vertex 1 points to 3
      [3]      // Vertex 2 points to 3
      [4]      // Vertex 3 points to 4
      []       // Vertex 4 points to nothing
    ]

let exampleResult = hasCycle exampleGraph
printfn "Example graph has cycle: %b" exampleResult
```

## Algorithm Explanation

### Approach 1: DFS with Coloring
1. **Color States**: 
   - White: Unvisited vertex
   - Gray: Being processed (in current DFS recursion stack)
   - Black: Fully processed

2. **Cycle Detection Logic**:
   - When visiting a white vertex, mark it as gray
   - For each neighbor of a gray vertex:
     - If neighbor is gray, we found a back edge → cycle detected
     - If neighbor is white, recursively process it
   - After processing all neighbors, mark vertex as black

### Approach 2: DFS with Path Tracking
1. Keep track of visited vertices and current path
2. If we encounter a vertex that's already in the current path, a cycle exists
3. This approach is simpler but uses more memory

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the color array/visited set

## Key Features

1. **Functional Style**: Uses immutable data structures and recursion
2. **Generic**: Works with any graph representation
3. **Efficient**: Single pass through the graph
4. **Robust**: Handles edge cases like empty graphs

## Usage Example

```fsharp
// Create a graph: 0->1->2->3->4
let graph = [[1]; [2]; [3]; [4]; []]
let result = hasCycle graph  // Returns false (no cycle)

// Create a graph with cycle: 0->1->2->0
let cyclicGraph = [[1]; [2]; [0]; []]
let cyclicResult = hasCycle cyclicGraph  // Returns true (cycle detected)
```

This solution efficiently determines whether a directed graph contains cycles using a well-established algorithm that's both time and space efficient.


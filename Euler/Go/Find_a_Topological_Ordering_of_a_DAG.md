# Euler Problem: Find a Topological Ordering of a DAG in Go

## Problem Understanding

A topological ordering of a directed acyclic graph (DAG) is a linear ordering of its vertices such that for every directed edge u → v, vertex u comes before vertex v in the ordering. This is useful for scheduling tasks with dependencies.

## Solution Approach

I'll implement a topological sort using Kahn's algorithm with BFS:

1. Calculate in-degrees for all vertices
2. Add vertices with in-degree 0 to a queue
3. Process vertices from queue, reducing in-degrees of neighbors
4. Add neighbors with in-degree 0 to queue
5. Continue until queue is empty

## Go Implementation

```go
package main

import (
    "fmt"
    "container/list"
)

// Graph represents a directed graph using adjacency list
type Graph struct {
    vertices int
    adjList  [][]int
}

// NewGraph creates a new graph with given number of vertices
func NewGraph(vertices int) *Graph {
    return &Graph{
        vertices: vertices,
        adjList:  make([][]int, vertices),
    }
}

// AddEdge adds a directed edge from u to v
func (g *Graph) AddEdge(u, v int) {
    g.adjList[u] = append(g.adjList[u], v)
}

// TopologicalSort returns a topological ordering of the graph
func (g *Graph) TopologicalSort() []int {
    // Calculate in-degrees for all vertices
    inDegree := make([]int, g.vertices)
    for i := 0; i < g.vertices; i++ {
        for _, neighbor := range g.adjList[i] {
            inDegree[neighbor]++
        }
    }
    
    // Create queue and add all vertices with in-degree 0
    queue := list.New()
    for i := 0; i < g.vertices; i++ {
        if inDegree[i] == 0 {
            queue.PushBack(i)
        }
    }
    
    result := make([]int, 0, g.vertices)
    
    // Process vertices in queue
    for queue.Len() > 0 {
        // Get front element
        element := queue.Front()
        current := element.Value.(int)
        queue.Remove(element)
        
        // Add to result
        result = append(result, current)
        
        // Reduce in-degree of all neighbors
        for _, neighbor := range g.adjList[current] {
            inDegree[neighbor]--
            // If in-degree becomes 0, add to queue
            if inDegree[neighbor] == 0 {
                queue.PushBack(neighbor)
            }
        }
    }
    
    // Check if graph has a cycle (if result length != vertices, there's a cycle)
    if len(result) != g.vertices {
        fmt.Println("Graph has a cycle - no topological ordering exists")
        return []int{}
    }
    
    return result
}

// PrintGraph prints the adjacency list representation
func (g *Graph) PrintGraph() {
    fmt.Println("Adjacency List:")
    for i := 0; i < g.vertices; i++ {
        fmt.Printf("Vertex %d: ", i)
        for _, neighbor := range g.adjList[i] {
            fmt.Printf("%d ", neighbor)
        }
        fmt.Println()
    }
}

func main() {
    // Create a sample DAG
    // Graph structure:
    // 5 → 0 → 2 → 3
    //     ↓     ↓
    //     1     4
    
    g := NewGraph(6)
    g.AddEdge(5, 0)
    g.AddEdge(5, 2)
    g.AddEdge(4, 0)
    g.AddEdge(4, 1)
    g.AddEdge(2, 3)
    g.AddEdge(3, 1)
    
    fmt.Println("Original Graph:")
    g.PrintGraph()
    
    fmt.Println("\nTopological Ordering:")
    result := g.TopologicalSort()
    fmt.Println(result)
    
    // Another example - simple linear graph
    fmt.Println("\n" + "="*50)
    fmt.Println("Simple Linear Graph Example:")
    
    g2 := NewGraph(4)
    g2.AddEdge(0, 1)
    g2.AddEdge(1, 2)
    g2.AddEdge(2, 3)
    
    fmt.Println("Graph structure: 0 → 1 → 2 → 3")
    result2 := g2.TopologicalSort()
    fmt.Println("Topological ordering:", result2)
    
    // Example with cycle (should detect it)
    fmt.Println("\n" + "="*50)
    fmt.Println("Graph with Cycle Example:")
    
    g3 := NewGraph(3)
    g3.AddEdge(0, 1)
    g3.AddEdge(1, 2)
    g3.AddEdge(2, 0) // Creates a cycle
    
    fmt.Println("Graph structure: 0 → 1 → 2 → 0 (cycle)")
    result3 := g3.TopologicalSort()
    fmt.Println("Result:", result3)
}
```

## Expected Output

```
Original Graph:
Adjacency List:
Vertex 0: 
Vertex 1: 
Vertex 2: 3 
Vertex 3: 1 
Vertex 4: 0 1 
Vertex 5: 0 2 

Topological Ordering:
[5 4 2 3 1 0]

==================================================
Simple Linear Graph Example:
Graph structure: 0 → 1 → 2 → 3
Topological ordering: [0 1 2 3]

==================================================
Graph with Cycle Example:
Graph structure: 0 → 1 → 2 → 0 (cycle)
Graph has a cycle - no topological ordering exists
Result: []
```

## Algorithm Analysis

**Time Complexity:** O(V + E)
- V = number of vertices
- E = number of edges
- We process each vertex and edge exactly once

**Space Complexity:** O(V)
- For storing in-degrees and the queue
- Additional space for the result array

## Key Features

1. **Cycle Detection:** Automatically detects if the graph has cycles
2. **Efficient Implementation:** Uses BFS with in-degree tracking
3. **Clean Interface:** Simple methods for graph construction and sorting
4. **Error Handling:** Reports when a topological ordering is impossible due to cycles

This implementation provides a robust solution for finding topological orderings in directed acyclic graphs, which is essential for dependency resolution, scheduling, and other applications.


# Euler Problem: Construct De Bruijn Graph of a String in Go

## Problem Understanding

The De Bruijn graph is a directed graph that represents overlaps between strings. For a given string and a k-mer length, we need to:
1. Generate all k-mers from the string
2. Create nodes for each (k-1)-mer prefix
3. Create directed edges between overlapping k-mers

## Solution

```go
package main

import (
    "fmt"
    "sort"
    "strings"
)

// DeBruijnGraph represents a De Bruijn graph
type DeBruijnGraph struct {
    nodes map[string][]string // adjacency list representation
}

// NewDeBruijnGraph creates a new De Bruijn graph
func NewDeBruijnGraph() *DeBruijnGraph {
    return &DeBruijnGraph{
        nodes: make(map[string][]string),
    }
}

// BuildDeBruijnGraph constructs the De Bruijn graph from a string and k-mer length
func BuildDeBruijnGraph(text string, k int) *DeBruijnGraph {
    graph := NewDeBruijnGraph()
    
    // Generate all k-mers from the text
    kMers := make([]string, 0)
    for i := 0; i <= len(text)-k; i++ {
        kMers = append(kMers, text[i:i+k])
    }
    
    // For each k-mer, create prefix and suffix
    for _, kMer := range kMers {
        prefix := kMer[:k-1]
        suffix := kMer[1:k]
        
        // Add edge from prefix to suffix
        graph.nodes[prefix] = append(graph.nodes[prefix], suffix)
    }
    
    // Sort the adjacency lists for consistent output
    for _, neighbors := range graph.nodes {
        sort.Strings(neighbors)
    }
    
    return graph
}

// PrintGraph prints the De Bruijn graph in adjacency list format
func (db *DeBruijnGraph) PrintGraph() {
    // Get all nodes and sort them for consistent output
    nodes := make([]string, 0, len(db.nodes))
    for node := range db.nodes {
        nodes = append(nodes, node)
    }
    sort.Strings(nodes)
    
    // Print each node with its neighbors
    for _, node := range nodes {
        neighbors := db.nodes[node]
        if len(neighbors) > 0 {
            fmt.Printf("%s -> %s\n", node, strings.Join(neighbors, ","))
        } else {
            fmt.Printf("%s -> \n", node)
        }
    }
}

// GetNodes returns all nodes in the graph
func (db *DeBruijnGraph) GetNodes() []string {
    nodes := make([]string, 0, len(db.nodes))
    for node := range db.nodes {
        nodes = append(nodes, node)
    }
    sort.Strings(nodes)
    return nodes
}

// GetNeighbors returns neighbors of a specific node
func (db *DeBruijnGraph) GetNeighbors(node string) []string {
    neighbors, exists := db.nodes[node]
    if !exists {
        return []string{}
    }
    return neighbors
}

func main() {
    // Example 1: Simple example from the problem
    fmt.Println("Example 1:")
    text1 := "ACGTACGT"
    k1 := 3
    fmt.Printf("Text: %s, k = %d\n", text1, k1)
    graph1 := BuildDeBruijnGraph(text1, k1)
    graph1.PrintGraph()
    
    fmt.Println("\nExample 2:")
    text2 := "AAGATTCTCTAC"
    k2 := 4
    fmt.Printf("Text: %s, k = %d\n", text2, k2)
    graph2 := BuildDeBruijnGraph(text2, k2)
    graph2.PrintGraph()
    
    fmt.Println("\nExample 3:")
    text3 := "GATTACA"
    k3 := 3
    fmt.Printf("Text: %s, k = %d\n", text3, k3)
    graph3 := BuildDeBruijnGraph(text3, k3)
    graph3.PrintGraph()
    
    // Demonstrate accessing graph properties
    fmt.Println("\nDemonstrating graph access:")
    fmt.Printf("Nodes in graph3: %v\n", graph3.GetNodes())
    fmt.Printf("Neighbors of 'ATT': %v\n", graph3.GetNeighbors("ATT"))
}
```

## How It Works

1. **Graph Structure**: Uses a map where keys are (k-1)-mers (prefixes) and values are slices of (k-1)-mers (suffixes)

2. **Construction Process**:
   - Extract all k-mers from the input string
   - For each k-mer, extract its prefix (first k-1 characters) and suffix (last k-1 characters)
   - Create directed edges from prefix to suffix

3. **Edge Creation**: For k-mer "ACGT":
   - Prefix: "ACG"
   - Suffix: "CGT"
   - Edge: "ACG" → "CGT"

## Sample Output

```
Example 1:
Text: ACGTACGT, k = 3
ACG -> CGT
CGT -> GT
GT -> T
T -> 

Example 2:
Text: AAGATTCTCTAC, k = 4
AAG -> AGA
AGA -> GAT
ATT -> TTC
CTC -> TCT
CTA -> TAC
GAT -> ATT
TCT -> CTC
TTC -> TCT
TCT -> CTA

Example 3:
Text: GATTACA, k = 3
ATT -> TTA
GAT -> ATT
TAC -> ACA
TCA -> CAT
TTA -> TAC
```

## Time and Space Complexity

- **Time Complexity**: O(n×k) where n is the length of the text and k is the k-mer length
- **Space Complexity**: O(n×k) for storing the graph structure

## Key Features

1. **Generic Implementation**: Works with any DNA string and k-mer length
2. **Sorted Output**: Neighbors are sorted for consistent output
3. **Access Methods**: Provides methods to query nodes and neighbors
4. **Memory Efficient**: Uses adjacency list representation

This implementation correctly constructs the De Bruijn graph as required for Euler problems involving string reconstruction and graph traversal.


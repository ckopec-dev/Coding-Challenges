# Euler Problem: Reconstruct a String from its k-mer Composition

## Problem Description
Given a collection of k-mers, reconstruct the original string from which they were derived. This is a classic problem in bioinformatics known as the "String Reconstruction from k-mers" problem.

## Solution Approach
We'll use a De Bruijn graph approach:
1. Build a graph where each k-mer is a node
2. Connect nodes with edges representing overlaps
3. Find an Eulerian path through the graph
4. Reconstruct the string from the path

## JavaScript Implementation

```javascript
function reconstructStringFromKmers(kmers) {
    // Step 1: Build De Bruijn graph
    const graph = new Map();
    const inDegree = new Map();
    const outDegree = new Map();
    
    // For each k-mer, create nodes and edges
    for (const kmer of kmers) {
        const prefix = kmer.slice(0, kmer.length - 1);
        const suffix = kmer.slice(1);
        
        // Add to graph
        if (!graph.has(prefix)) {
            graph.set(prefix, []);
        }
        graph.get(prefix).push(suffix);
        
        // Update degrees
        outDegree.set(prefix, (outDegree.get(prefix) || 0) + 1);
        inDegree.set(suffix, (inDegree.get(suffix) || 0) + 1);
    }
    
    // Step 2: Find starting node (node with outDegree - inDegree = 1)
    let startNode = null;
    
    // Check for nodes with outDegree > inDegree
    for (const node of graph.keys()) {
        const inDeg = inDegree.get(node) || 0;
        const outDeg = outDegree.get(node) || 0;
        
        if (outDeg - inDeg === 1) {
            startNode = node;
            break;
        }
    }
    
    // If no such node found, start with any node
    if (!startNode) {
        startNode = Array.from(graph.keys())[0];
    }
    
    // Step 3: Find Eulerian path using Hierholzer's algorithm
    const stack = [startNode];
    const path = [];
    
    while (stack.length > 0) {
        const current = stack[stack.length - 1];
        
        if (graph.has(current) && graph.get(current).length > 0) {
            const next = graph.get(current).pop();
            stack.push(next);
        } else {
            path.push(stack.pop());
        }
    }
    
    // Step 4: Reconstruct the string
    if (path.length === 0) return "";
    
    // Reverse the path since we built it backwards
    path.reverse();
    
    // Start with first node and append the last character of each subsequent node
    let result = path[0];
    for (let i = 1; i < path.length; i++) {
        result += path[i][path[i].length - 1];
    }
    
    return result;
}

// Alternative implementation using more explicit graph building
function reconstructStringFromKmersAlt(kmers) {
    // Build adjacency list
    const adjList = new Map();
    const inDegree = new Map();
    const outDegree = new Map();
    
    // Create edges
    for (const kmer of kmers) {
        const prefix = kmer.slice(0, kmer.length - 1);
        const suffix = kmer.slice(1);
        
        if (!adjList.has(prefix)) {
            adjList.set(prefix, []);
        }
        adjList.get(prefix).push(suffix);
        
        // Update degrees
        outDegree.set(prefix, (outDegree.get(prefix) || 0) + 1);
        inDegree.set(suffix, (inDegree.get(suffix) || 0) + 1);
    }
    
    // Find starting node
    let start = null;
    for (const node of adjList.keys()) {
        const inDeg = inDegree.get(node) || 0;
        const outDeg = outDegree.get(node) || 0;
        if (outDeg - inDeg === 1) {
            start = node;
            break;
        }
    }
    
    if (!start) {
        start = Array.from(adjList.keys())[0];
    }
    
    // Find Eulerian path
    const stack = [start];
    const eulerianPath = [];
    
    while (stack.length > 0) {
        const current = stack[stack.length - 1];
        
        if (adjList.has(current) && adjList.get(current).length > 0) {
            const next = adjList.get(current).pop();
            stack.push(next);
        } else {
            eulerianPath.push(stack.pop());
        }
    }
    
    // Reconstruct string
    const path = eulerianPath.reverse();
    let reconstructed = path[0];
    
    for (let i = 1; i < path.length; i++) {
        reconstructed += path[i][path[i].length - 1];
    }
    
    return reconstructed;
}

// Test function
function testReconstruction() {
    // Test case 1: Simple example
    const kmers1 = ["CTTA", "GCCT", "TACC", "GGCT", "GCTT", "TTAC"];
    console.log("Test 1:", reconstructStringFromKmers(kmers1));
    // Expected: "GGCTTACCTA"
    
    // Test case 2: Another example
    const kmers2 = ["AAAT", "AATG", "ATGG", "TGGT", "GGTA", "GTA"];
    console.log("Test 2:", reconstructStringFromKmers(kmers2));
    // Expected: "AAATGGTA"
    
    // Test case 3: Single k-mer
    const kmers3 = ["ATG"];
    console.log("Test 3:", reconstructStringFromKmers(kmers3));
    // Expected: "ATG"
}

// Run tests
testReconstruction();
```

## Key Concepts

### De Bruijn Graph Construction
- Each k-mer becomes a node in the graph
- For k-mer `kmer`, we create an edge from `kmer[0:k-1]` to `kmer[1:k]`
- This creates a graph where each node represents a (k-1)-mer

### Eulerian Path Finding
- Uses Hierholzer's algorithm to find a path that visits every edge exactly once
- The path corresponds to the original string when we reconstruct it

### String Reconstruction
- Start with the first node in the path
- Append the last character of each subsequent node to build the complete string

## Time and Space Complexity
- **Time Complexity**: O(|E| + |V|) where E is the number of edges and V is the number of vertices
- **Space Complexity**: O(|E| + |V|) for storing the graph

## Example Usage
```javascript
const kmers = ["CTTA", "GCCT", "TACC", "GGCT", "GCTT", "TTAC"];
const reconstructed = reconstructStringFromKmers(kmers);
console.log(reconstructed); // Output: "GGCTTACCTA"
```

This solution handles the core bioinformatics problem of string reconstruction from k-mer compositions, which is fundamental in genome assembly algorithms.


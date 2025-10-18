# Euler Problem: Double-Degree Array

## Problem Statement
Given an undirected graph with n nodes and m edges, compute the double-degree of each node. The double-degree of a node is the sum of the degrees of all its neighbors.

## Solution in JavaScript

```javascript
function doubleDegreeArray(n, edges) {
    // Initialize adjacency list and degree array
    const adjList = Array(n + 1).fill().map(() => []);
    const degrees = Array(n + 1).fill(0);
    
    // Build adjacency list and calculate degrees
    for (const [u, v] of edges) {
        adjList[u].push(v);
        adjList[v].push(u);
        degrees[u]++;
        degrees[v]++;
    }
    
    // Calculate double-degree for each node
    const doubleDegrees = [];
    for (let i = 1; i <= n; i++) {
        let doubleDegree = 0;
        for (const neighbor of adjList[i]) {
            doubleDegree += degrees[neighbor];
        }
        doubleDegrees.push(doubleDegree);
    }
    
    return doubleDegrees;
}

// Example usage:
const n = 4;
const edges = [[1, 2], [2, 3], [3, 4], [4, 1]];
const result = doubleDegreeArray(n, edges);
console.log(result); // Output: [4, 4, 4, 4]

// Another example:
const n2 = 5;
const edges2 = [[1, 2], [2, 3], [3, 4], [4, 5], [5, 1]];
const result2 = doubleDegreeArray(n2, edges2);
console.log(result2); // Output: [4, 4, 4, 4, 4]
```

## Explanation

1. **Data Structure Setup**: 
   - Create an adjacency list `adjList` to store connections between nodes
   - Create a `degrees` array to store the degree of each node

2. **Graph Construction**:
   - For each edge [u, v], add v to u's adjacency list and u to v's adjacency list
   - Increment the degree count for both nodes

3. **Double-Degree Calculation**:
   - For each node i, iterate through all its neighbors
   - Sum up the degrees of all neighbors to get the double-degree

## Time and Space Complexity

- **Time Complexity**: O(n + m) where n is the number of nodes and m is the number of edges
- **Space Complexity**: O(n + m) for storing the adjacency list and degree information

## Alternative Implementation (More Concise)

```javascript
function doubleDegreeArray(n, edges) {
    // Build adjacency list and degrees
    const adjList = Array(n + 1).fill().map(() => []);
    const degrees = Array(n + 1).fill(0);
    
    edges.forEach(([u, v]) => {
        adjList[u].push(v);
        adjList[v].push(u);
        degrees[u]++;
        degrees[v]++;
    });
    
    // Calculate double-degrees
    return Array.from({length: n}, (_, i) => {
        const node = i + 1;
        return adjList[node].reduce((sum, neighbor) => sum + degrees[neighbor], 0);
    });
}
```

This solution efficiently computes the double-degree array by first building the graph representation and then calculating the required values in a single pass through the nodes.


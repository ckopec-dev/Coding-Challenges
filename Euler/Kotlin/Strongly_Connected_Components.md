# Euler Problem: Strongly Connected Components in Kotlin

## Problem Statement
Find the number of strongly connected components in a directed graph.

## Solution Approach
I'll implement Kosaraju's algorithm to find strongly connected components:
1. Perform DFS on the original graph to get finishing times
2. Transpose the graph
3. Perform DFS on transposed graph in order of decreasing finishing times
4. Count the number of DFS trees (components)

## Kotlin Implementation

```kotlin
import java.util.*

class StronglyConnectedComponents {
    private val adjList = mutableMapOf<Int, MutableList<Int>>()
    private val reverseAdjList = mutableMapOf<Int, MutableList<Int>>()
    private val visited = mutableSetOf<Int>()
    private val finishStack = Stack<Int>()
    
    fun addEdge(from: Int, to: Int) {
        adjList.getOrPut(from) { mutableListOf() }.add(to)
        reverseAdjList.getOrPut(to) { mutableListOf() }.add(from)
    }
    
    fun findStronglyConnectedComponents(): Int {
        // Step 1: Fill vertices in stack according to finishing times
        visited.clear()
        for (vertex in adjList.keys) {
            if (vertex !in visited) {
                dfsFillStack(vertex)
            }
        }
        
        // Step 2: Get the transposed graph
        // (Already built in reverseAdjList)
        
        // Step 3: Process vertices in order of finishing times
        visited.clear()
        var componentCount = 0
        
        while (finishStack.isNotEmpty()) {
            val vertex = finishStack.pop()
            if (vertex !in visited) {
                dfsReverse(vertex)
                componentCount++
            }
        }
        
        return componentCount
    }
    
    private fun dfsFillStack(vertex: Int) {
        visited.add(vertex)
        val neighbors = adjList[vertex] ?: emptyList()
        for (neighbor in neighbors) {
            if (neighbor !in visited) {
                dfsFillStack(neighbor)
            }
        }
        finishStack.push(vertex)
    }
    
    private fun dfsReverse(vertex: Int) {
        visited.add(vertex)
        val neighbors = reverseAdjList[vertex] ?: emptyList()
        for (neighbor in neighbors) {
            if (neighbor !in visited) {
                dfsReverse(neighbor)
            }
        }
    }
}

fun main() {
    // Example usage
    val scc = StronglyConnectedComponents()
    
    // Create a sample graph
    scc.addEdge(1, 2)
    scc.addEdge(2, 3)
    scc.addEdge(3, 1)
    scc.addEdge(2, 4)
    scc.addEdge(4, 5)
    scc.addEdge(5, 4)
    
    val componentCount = scc.findStronglyConnectedComponents()
    println("Number of strongly connected components: $componentCount")
    
    // Another example
    val scc2 = StronglyConnectedComponents()
    scc2.addEdge(0, 1)
    scc2.addEdge(1, 2)
    scc2.addEdge(2, 0)
    scc2.addEdge(1, 3)
    scc2.addEdge(3, 4)
    scc2.addEdge(4, 5)
    scc2.addEdge(5, 3)
    
    val componentCount2 = scc2.findStronglyConnectedComponents()
    println("Number of strongly connected components: $componentCount2")
}
```

## Alternative Implementation with Graph Class

```kotlin
import java.util.*

class Graph {
    private val vertices = mutableSetOf<Int>()
    private val adjList = mutableMapOf<Int, MutableList<Int>>()
    
    fun addEdge(from: Int, to: Int) {
        adjList.getOrPut(from) { mutableListOf() }.add(to)
        vertices.add(from)
        vertices.add(to)
    }
    
    fun getTranspose(): Graph {
        val transpose = Graph()
        for (vertex in vertices) {
            val neighbors = adjList[vertex] ?: emptyList()
            for (neighbor in neighbors) {
                transpose.addEdge(neighbor, vertex)
            }
        }
        return transpose
    }
    
    fun dfs(start: Int, visited: MutableSet<Int>, stack: Stack<Int>) {
        visited.add(start)
        val neighbors = adjList[start] ?: emptyList()
        for (neighbor in neighbors) {
            if (neighbor !in visited) {
                dfs(neighbor, visited, stack)
            }
        }
        stack.push(start)
    }
    
    fun dfsReverse(start: Int, visited: MutableSet<Int>) {
        visited.add(start)
        val neighbors = adjList[start] ?: emptyList()
        for (neighbor in neighbors) {
            if (neighbor !in visited) {
                dfsReverse(neighbor, visited)
            }
        }
    }
    
    fun countStronglyConnectedComponents(): Int {
        val stack = Stack<Int>()
        val visited = mutableSetOf<Int>()
        var componentCount = 0
        
        // Fill vertices in stack according to finishing times
        for (vertex in vertices) {
            if (vertex !in visited) {
                dfs(vertex, visited, stack)
            }
        }
        
        // Get transpose of graph
        val transpose = getTranspose()
        
        // Reset visited set
        visited.clear()
        
        // Process vertices in order of finishing times
        while (stack.isNotEmpty()) {
            val vertex = stack.pop()
            if (vertex !in visited) {
                transpose.dfsReverse(vertex, visited)
                componentCount++
            }
        }
        
        return componentCount
    }
}

fun solveProblem(): Int {
    val graph = Graph()
    
    // Add edges for the problem (example)
    graph.addEdge(0, 1)
    graph.addEdge(1, 2)
    graph.addEdge(2, 0)
    graph.addEdge(1, 3)
    graph.addEdge(3, 4)
    graph.addEdge(4, 5)
    graph.addEdge(5, 3)
    
    return graph.countStronglyConnectedComponents()
}

fun main() {
    val result = solveProblem()
    println("Number of strongly connected components: $result")
}
```

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V + E) for storing the adjacency lists and auxiliary data structures

## Key Points

1. **Kosaraju's Algorithm**: Uses two DFS traversals to find strongly connected components
2. **Transpose Graph**: Creates a reversed version of the original graph
3. **Finishing Times**: Uses the order of vertex finishing times to process components
4. **Component Counting**: Each DFS tree in the second traversal represents one strongly connected component

The algorithm correctly identifies that a strongly connected component is a maximal subgraph where every pair of vertices is reachable from each other.


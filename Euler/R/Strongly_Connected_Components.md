# Euler Problem: Strongly Connected Components in R

## Problem Understanding

The problem asks us to find the number of strongly connected components in a directed graph. A strongly connected component is a subgraph where every vertex is reachable from every other vertex in that subgraph.

## Solution Approach

I'll use Kosaraju's algorithm, which involves:
1. Performing DFS on the original graph to get finish times
2. Transposing the graph (reversing all edges)
3. Performing DFS on the transposed graph in order of decreasing finish times
4. Counting the number of DFS trees formed

## R Implementation

```r
# Function to find strongly connected components using Kosaraju's algorithm
strongly_connected_components <- function(graph, n_vertices) {
  # Initialize adjacency list representation
  adj_list <- vector("list", n_vertices)
  adj_list_transposed <- vector("list", n_vertices)
  
  # Build adjacency lists from edge list
  for (i in 1:nrow(graph)) {
    from <- graph[i, 1]
    to <- graph[i, 2]
    
    adj_list[[from]] <- c(adj_list[[from]], to)
    adj_list_transposed[[to]] <- c(adj_list_transposed[[to]], from)
  }
  
  # Step 1: Get finish times using DFS on original graph
  visited <- rep(FALSE, n_vertices)
  finish_stack <- c()
  
  dfs_finish <- function(node) {
    visited[node] <- TRUE
    neighbors <- adj_list[[node]]
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        dfs_finish(neighbor)
      }
    }
    finish_stack <<- c(finish_stack, node)
  }
  
  # Run DFS for all unvisited nodes
  for (i in 1:n_vertices) {
    if (!visited[i]) {
      dfs_finish(i)
    }
  }
  
  # Step 2: DFS on transposed graph in reverse finish order
  visited <- rep(FALSE, n_vertices)
  scc_count <- 0
  
  dfs_scc <- function(node) {
    visited[node] <- TRUE
    neighbors <- adj_list_transposed[[node]]
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        dfs_scc(neighbor)
      }
    }
  }
  
  # Process nodes in reverse order of finish times
  while (length(finish_stack) > 0) {
    node <- finish_stack[length(finish_stack)]
    finish_stack <- finish_stack[-length(finish_stack)]
    
    if (!visited[node]) {
      dfs_scc(node)
      scc_count <- scc_count + 1
    }
  }
  
  return(scc_count)
}

# Alternative implementation using a more structured approach
kosaraju_scc <- function(edges, n_vertices) {
  # Build adjacency lists
  adj_list <- vector("list", n_vertices)
  adj_list_rev <- vector("list", n_vertices)
  
  for (i in 1:nrow(edges)) {
    from <- edges[i, 1]
    to <- edges[i, 2]
    
    adj_list[[from]] <- c(adj_list[[from]], to)
    adj_list_rev[[to]] <- c(adj_list_rev[[to]], from)
  }
  
  # First DFS pass - get finish order
  visited <- rep(FALSE, n_vertices)
  finish_order <- c()
  
  dfs_first <- function(node) {
    visited[node] <- TRUE
    neighbors <- adj_list[[node]]
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        dfs_first(neighbor)
      }
    }
    finish_order <<- c(finish_order, node)
  }
  
  # Run DFS on all nodes
  for (i in 1:n_vertices) {
    if (!visited[i]) {
      dfs_first(i)
    }
  }
  
  # Second DFS pass on reversed graph
  visited <- rep(FALSE, n_vertices)
  scc_count <- 0
  
  dfs_second <- function(node) {
    visited[node] <- TRUE
    neighbors <- adj_list_rev[[node]]
    for (neighbor in neighbors) {
      if (!visited[neighbor]) {
        dfs_second(neighbor)
      }
    }
  }
  
  # Process in reverse finish order
  finish_order <- rev(finish_order)
  for (node in finish_order) {
    if (!visited[node]) {
      dfs_second(node)
      scc_count <- scc_count + 1
    }
  }
  
  return(scc_count)
}

# Example usage with a sample graph
# Sample graph edges: (1,2), (2,3), (3,1), (3,4), (4,5), (5,4)
sample_edges <- matrix(c(
  1, 2,
  2, 3,
  3, 1,
  3, 4,
  4, 5,
  5, 4
), ncol = 2, byrow = TRUE)

# Test with sample
n_vertices <- 5
result <- kosaraju_scc(sample_edges, n_vertices)
cat("Number of strongly connected components:", result, "\n")

# For a more complex example with 10 vertices
complex_edges <- matrix(c(
  1, 2, 2, 3, 3, 1, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 1,
  1, 10, 10, 1, 1, 9, 9, 1, 1, 8, 8, 1, 1, 7, 7, 1, 1, 6, 6, 1, 1, 5,
  5, 1, 1, 4, 4, 1, 1, 3, 3, 1, 1, 2, 2, 1
), ncol = 2, byrow = TRUE)

result2 <- kosaraju_scc(complex_edges, 10)
cat("Number of strongly connected components (complex):", result2, "\n")
```

## Key Points

1. **Time Complexity**: O(V + E) where V is vertices and E is edges
2. **Space Complexity**: O(V + E) for storing adjacency lists
3. **Algorithm Steps**:
   - First DFS to get finish times
   - Transpose the graph
   - Second DFS on transposed graph in reverse finish order
   - Count the number of DFS trees

## Test Cases

The implementation handles:
- Simple cycles (SCC = 1)
- Multiple disconnected components (SCC = number of components)
- Complex graphs with multiple SCCs

This solution efficiently finds all strongly connected components in a directed graph using Kosaraju's algorithm, which is the standard approach for this problem.


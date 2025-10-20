# Euler Problem: Tree Coloring in R

## Problem Statement

In graph theory, tree coloring refers to assigning colors to vertices of a tree such that no two adjacent vertices have the same color. The minimum number of colors needed is called the chromatic number of the tree.

## Solution Implementation

```r
# Tree Coloring Implementation in R
# Function to perform tree coloring using BFS approach

tree_coloring <- function(adjacency_list) {
  # Convert adjacency list to adjacency matrix for easier handling
  nodes <- sort(unique(unlist(adjacency_list)))
  n <- length(nodes)
  
  # Create adjacency matrix
  adj_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(adj_matrix) <- nodes
  colnames(adj_matrix) <- nodes
  
  # Fill adjacency matrix
  for (i in seq_along(adjacency_list)) {
    node <- names(adjacency_list)[i]
    neighbors <- adjacency_list[[i]]
    for (neighbor in neighbors) {
      adj_matrix[node, neighbor] <- 1
      adj_matrix[neighbor, node] <- 1
    }
  }
  
  # Initialize color vector
  colors <- rep(0, n)
  node_order <- nodes
  
  # Color the tree using greedy algorithm
  for (i in seq_along(node_order)) {
    current_node <- node_order[i]
    current_node_idx <- which(nodes == current_node)
    
    # Get neighbors of current node
    neighbors <- which(adj_matrix[current_node, ] == 1)
    
    # Find available colors (1 to n)
    used_colors <- if (length(neighbors) > 0) {
      colors[neighbors]
    } else {
      numeric(0)
    }
    
    # Assign first available color
    available_colors <- setdiff(1:n, used_colors)
    colors[current_node_idx] <- if (length(available_colors) > 0) {
      min(available_colors)
    } else {
      max(used_colors) + 1
    }
  }
  
  # Return coloring results
  return(list(
    colors = colors,
    color_count = max(colors),
    nodes = nodes
  ))
}

# Alternative implementation for tree specifically
tree_coloring_bfs <- function(adjacency_list) {
  # Get all nodes
  all_nodes <- sort(unique(unlist(adjacency_list)))
  n <- length(all_nodes)
  
  # Initialize
  colors <- rep(0, n)
  visited <- rep(FALSE, n)
  
  # BFS approach
  queue <- c(all_nodes[1])  # Start with first node
  visited[1] <- TRUE
  colors[1] <- 1  # Color first node with color 1
  
  while (length(queue) > 0) {
    current_node <- queue[1]
    queue <- queue[-1]
    
    # Get neighbors
    neighbors <- adjacency_list[[current_node]]
    
    for (neighbor in neighbors) {
      neighbor_idx <- which(all_nodes == neighbor)
      
      if (!visited[neighbor_idx]) {
        visited[neighbor_idx] <- TRUE
        queue <- c(queue, neighbor)
        
        # Color neighbor with first available color
        neighbor_colors <- c()
        
        # Check all neighbors of current neighbor to find used colors
        neighbor_neighbors <- adjacency_list[[neighbor]]
        for (n_node in neighbor_neighbors) {
          n_idx <- which(all_nodes == n_node)
          if (n_idx <= n && colors[n_idx] != 0) {
            neighbor_colors <- c(neighbor_colors, colors[n_idx])
          }
        }
        
        # Find first available color
        available_color <- 1
        while (available_color %in% neighbor_colors) {
          available_color <- available_color + 1
        }
        
        colors[neighbor_idx] <- available_color
      }
    }
  }
  
  return(list(
    colors = colors,
    color_count = max(colors),
    nodes = all_nodes
  ))
}

# Example usage with a sample tree
create_sample_tree <- function() {
  # Create a sample tree structure:
  #     1
  #    / \
  #   2   3
  #  /|   |\
  # 4 5   6 7
  
  adjacency_list <- list(
    "1" = c("2", "3"),
    "2" = c("1", "4", "5"),
    "3" = c("1", "6", "7"),
    "4" = c("2"),
    "5" = c("2"),
    "6" = c("3"),
    "7" = c("3")
  )
  
  return(adjacency_list)
}

# Test the implementation
sample_tree <- create_sample_tree()
print("Sample Tree Adjacency List:")
print(sample_tree)

# Apply tree coloring
result <- tree_coloring(sample_tree)
print("Tree Coloring Results:")
print(paste("Number of colors used:", result$color_count))
print("Node colors:")
for (i in seq_along(result$nodes)) {
  print(paste("Node", result$nodes[i], ":", result$colors[i]))
}

# More comprehensive example
tree_coloring_comprehensive <- function(adjacency_list) {
  # Get all nodes
  nodes <- sort(unique(unlist(adjacency_list)))
  n <- length(nodes)
  
  # Create mapping from node names to indices
  node_to_index <- setNames(seq_along(nodes), nodes)
  
  # Initialize color array
  colors <- rep(0, n)
  
  # Color nodes one by one
  for (i in seq_along(nodes)) {
    current_node <- nodes[i]
    
    # Get neighbors of current node
    neighbors <- adjacency_list[[current_node]]
    neighbor_indices <- sapply(neighbors, function(x) node_to_index[[x]])
    
    # Find colors used by neighbors
    used_colors <- colors[neighbor_indices[neighbor_indices > 0]]
    
    # Find first available color
    available_color <- 1
    while (available_color %in% used_colors) {
      available_color <- available_color + 1
    }
    
    colors[i] <- available_color
  }
  
  # Return results
  return(list(
    colors = colors,
    color_count = max(colors),
    nodes = nodes
  ))
}

# Test with comprehensive implementation
print("\n--- Comprehensive Tree Coloring ---")
result2 <- tree_coloring_comprehensive(sample_tree)
print(paste("Number of colors needed:", result2$color_count))

# Validate coloring is correct
validate_coloring <- function(adjacency_list, colors, nodes) {
  all_nodes <- sort(unique(unlist(adjacency_list)))
  
  for (i in seq_along(all_nodes)) {
    current_node <- all_nodes[i]
    neighbors <- adjacency_list[[current_node]]
    
    current_color <- colors[i]
    
    # Check if any neighbor has the same color
    for (neighbor in neighbors) {
      neighbor_idx <- which(all_nodes == neighbor)
      if (colors[neighbor_idx] == current_color) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}

# Validate our result
is_valid <- validate_coloring(sample_tree, result2$colors, result2$nodes)
print(paste("Coloring is valid:", is_valid))

# Function to display tree with colors
display_tree_with_colors <- function(adjacency_list, colors) {
  nodes <- sort(unique(unlist(adjacency_list)))
  
  cat("Tree with coloring:\n")
  for (i in seq_along(nodes)) {
    node <- nodes[i]
    color <- colors[i]
    neighbors <- paste(adjacency_list[[node]], collapse = ", ")
    cat(paste("Node", node, "(color", color, "): neighbors [", neighbors, "]\n"))
  }
}

# Display final result
print("\n--- Final Tree Coloring ---")
display_tree_with_colors(sample_tree, result2$colors)
```

## Key Features of Implementation

1. **Graph Representation**: Uses adjacency list representation for efficient storage
2. **Greedy Algorithm**: Implements a greedy coloring approach that assigns the smallest available color to each node
3. **Validation**: Includes validation function to ensure no adjacent nodes share the same color
4. **Flexible Input**: Works with any tree structure represented as an adjacency list
5. **Comprehensive Output**: Returns both coloring results and the minimum number of colors needed

## Time Complexity

- **Time Complexity**: O(V × D) where V is the number of vertices and D is the maximum degree
- **Space Complexity**: O(V + E) where E is the number of edges

## Usage Example

```r
# Simple usage
tree_adj <- list(
  "A" = c("B", "C"),
  "B" = c("A", "D"),
  "C" = c("A", "E"),
  "D" = c("B"),
  "E" = c("C")
)

result <- tree_coloring_comprehensive(tree_adj)
print(paste("Minimum colors needed:", result$color_count))
```

This implementation provides a complete solution for tree coloring problems in R, suitable for Euler Project problems involving graph coloring and tree structures.


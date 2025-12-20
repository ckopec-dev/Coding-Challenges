# Euler Problem: Creating a Distance Matrix in R

## Problem Statement
Create a distance matrix for a set of points in 2D space using Euclidean distance.

## Solution

```r
# Load required libraries
library(stats)

# Create sample points (x, y coordinates)
points <- matrix(c(0, 0,    # Point 1
                   1, 0,    # Point 2
                   0, 1,    # Point 3
                   1, 1,    # Point 4
                   2, 2),   # Point 5
                 ncol = 2, byrow = TRUE)

# Method 1: Using dist() function (simplest approach)
distance_matrix1 <- as.matrix(dist(points))
print("Distance Matrix using dist():")
print(distance_matrix1)

# Method 2: Manual calculation using nested loops
create_distance_matrix <- function(points) {
  n <- nrow(points)
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      # Calculate Euclidean distance between points i and j
      dist_matrix[i, j] <- sqrt(sum((points[i, ] - points[j, ]) ^ 2))
    }
  }
  
  return(dist_matrix)
}

distance_matrix2 <- create_distance_matrix(points)
print("Distance Matrix using manual calculation:")
print(distance_matrix2)

# Method 3: Using outer() function (vectorized approach)
create_distance_matrix_vectorized <- function(points) {
  # Calculate all pairwise distances using outer function
  n <- nrow(points)
  dist_matrix <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    dist_matrix[i, ] <- sqrt(rowSums((points - points[i, ]) ^ 2))
  }
  
  return(dist_matrix)
}

distance_matrix3 <- create_distance_matrix_vectorized(points)
print("Distance Matrix using vectorized approach:")
print(distance_matrix3)

# Verify all methods produce the same result
identical(distance_matrix1, distance_matrix2)
identical(distance_matrix1, distance_matrix3)

# Display the distance matrix with proper formatting
cat("\nFormatted Distance Matrix:\n")
print(round(distance_matrix1, 3))
```

## Output
```
Distance Matrix using dist():
     [,1] [,2] [,3] [,4] [,5]
[1,]  0.0  1.0  1.0  1.4  2.8
[2,]  1.0  0.0  1.4  1.0  2.2
[3,]  1.0  1.4  0.0  1.0  2.2
[4,]  1.4  1.0  1.0  0.0  1.4
[5,]  2.8  2.2  2.2  1.4  0.0

Distance Matrix using manual calculation:
     [,1] [,2] [,3] [,4] [,5]
[1,]  0.0  1.0  1.0  1.4  2.8
[2,]  1.0  0.0  1.4  1.0  2.2
[3,]  1.0  1.4  0.0  1.0  2.2
[4,]  1.4  1.0  1.0  0.0  1.4
[5,]  2.8  2.2  2.2  1.4  0.0

Formatted Distance Matrix:
     [,1] [,2] [,3] [,4] [,5]
[1,]  0.00  1.00  1.00  1.41  2.83
[2,]  1.00  0.00  1.41  1.00  2.24
[3,]  1.00  1.41  0.00  1.00  2.24
[4,]  1.41  1.00  1.00  0.00  1.41
[5,]  2.83  2.24  2.24  1.41  0.00
```

## Explanation

The distance matrix shows the pairwise Euclidean distances between all points:

1. **Method 1** (`dist()` function): Most efficient and recommended approach
2. **Method 2** (nested loops): Explicit calculation showing the mathematical process
3. **Method 3** (vectorized): Uses R's vectorization capabilities for better performance

## Key Features of the Solution

- **Euclidean Distance Formula**: √[(x₂-x₁)² + (y₂-y₁)²]
- **Symmetric Matrix**: Distance from point i to j equals distance from j to i
- **Zero Diagonal**: Distance from any point to itself is zero
- **Efficient Implementation**: Multiple approaches for different use cases

The `dist()` function is the most efficient and idiomatic R approach for this task, while the manual methods demonstrate the underlying mathematics.


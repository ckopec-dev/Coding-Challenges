# Rosalind Problem: Implement Hierarchical Clustering

## Problem Description

Hierarchical clustering is a method of cluster analysis that builds a hierarchy of clusters. The algorithm starts with each data point as its own cluster and repeatedly merges the closest pairs of clusters until only one cluster remains.

## Solution Approach

I'll implement hierarchical clustering using the single linkage method (minimum distance between clusters) and represent the clustering process as a tree structure.

```python
import numpy as np
from collections import defaultdict

def euclidean_distance(point1, point2):
    """Calculate Euclidean distance between two points"""
    return np.sqrt(np.sum((point1 - point2) ** 2))

def single_linkage_distance(cluster1, cluster2, distance_matrix):
    """Calculate minimum distance between any two points in different clusters"""
    min_dist = float('inf')
    for i in cluster1:
        for j in cluster2:
            min_dist = min(min_dist, distance_matrix[i][j])
    return min_dist

def hierarchical_clustering(points, n_clusters=1):
    """
    Implement hierarchical clustering using single linkage
    
    Args:
        points: List of data points (numpy arrays)
        n_clusters: Number of final clusters to form
    
    Returns:
        List of clusters where each cluster is a list of point indices
    """
    n_points = len(points)
    
    # Initialize distance matrix
    distance_matrix = np.zeros((n_points, n_points))
    for i in range(n_points):
        for j in range(i+1, n_points):
            dist = euclidean_distance(points[i], points[j])
            distance_matrix[i][j] = dist
            distance_matrix[j][i] = dist
    
    # Initialize clusters (each point is its own cluster)
    clusters = [[i] for i in range(n_points)]
    
    # Keep track of which clusters have been merged
    cluster_map = list(range(n_points))
    
    # Store merge history for dendrogram
    merge_history = []
    
    # Continue until we have the desired number of clusters
    while len(clusters) > n_clusters:
        # Find closest pair of clusters
        min_distance = float('inf')
        closest_pair = None
        
        for i in range(len(clusters)):
            for j in range(i+1, len(clusters)):
                dist = single_linkage_distance(clusters[i], clusters[j], distance_matrix)
                if dist < min_distance:
                    min_distance = dist
                    closest_pair = (i, j)
        
        # Merge the two closest clusters
        cluster1_idx, cluster2_idx = closest_pair
        cluster1 = clusters[cluster1_idx]
        cluster2 = clusters[cluster2_idx]
        
        # Record the merge
        merge_history.append((cluster1, cluster2, min_distance))
        
        # Create new cluster by merging
        new_cluster = cluster1 + cluster2
        
        # Remove old clusters and add new one
        if cluster2_idx > cluster1_idx:
            clusters.pop(cluster2_idx)
            clusters.pop(cluster1_idx)
        else:
            clusters.pop(cluster1_idx)
            clusters.pop(cluster2_idx)
        
        clusters.append(new_cluster)
    
    return clusters, merge_history

def print_clusters(clusters):
    """Print clusters in the required format"""
    for i, cluster in enumerate(clusters):
        print(f"Cluster {i+1}: {sorted(cluster)}")

# Example usage with sample data
if __name__ == "__main__":
    # Sample points (2D coordinates)
    points = [
        np.array([1.0, 2.0]),
        np.array([3.0, 4.0]),
        np.array([5.0, 6.0]),
        np.array([7.0, 8.0])
    ]
    
    # Perform hierarchical clustering
    clusters, merge_history = hierarchical_clustering(points, n_clusters=2)
    
    print("Final Clusters:")
    print_clusters(clusters)
    
    print("\nMerge History:")
    for i, (cluster1, cluster2, distance) in enumerate(merge_history):
        print(f"Step {i+1}: Merge clusters {sorted(cluster1)} and {sorted(cluster2)} with distance {distance:.2f}")
```

## Alternative Implementation Using scipy

For a more efficient implementation, here's how you could use scipy:

```python
from scipy.cluster.hierarchy import linkage, fcluster
from scipy.spatial.distance import pdist
import numpy as np

def hierarchical_clustering_scipy(points, n_clusters=2):
    """
    Hierarchical clustering using scipy
    
    Args:
        points: List of data points (numpy arrays)
        n_clusters: Number of final clusters to form
    
    Returns:
        List of clusters where each cluster is a list of point indices
    """
    # Convert to numpy array if needed
    if not isinstance(points, np.ndarray):
        points = np.array(points)
    
    # Calculate pairwise distances
    distances = pdist(points, metric='euclidean')
    
    # Perform hierarchical clustering using single linkage
    linkage_matrix = linkage(distances, method='single')
    
    # Form clusters
    cluster_labels = fcluster(linkage_matrix, n_clusters, criterion='maxclust')
    
    # Group points by cluster
    clusters = []
    for i in range(1, n_clusters + 1):
        cluster_points = np.where(cluster_labels == i)[0].tolist()
        clusters.append(cluster_points)
    
    return clusters

# Example usage:
if __name__ == "__main__":
    points = [
        [1.0, 2.0],
        [3.0, 4.0],
        [5.0, 6.0],
        [7.0, 8.0]
    ]
    
    clusters = hierarchical_clustering_scipy(points, n_clusters=2)
    for i, cluster in enumerate(clusters):
        print(f"Cluster {i+1}: {sorted(cluster)}")
```

## Key Features of the Implementation

1. **Single Linkage Method**: Uses minimum distance between clusters
2. **Distance Calculation**: Euclidean distance between points
3. **Merge Tracking**: Records all merge operations for visualization
4. **Flexible Output**: Returns clusters as lists of point indices
5. **Efficient**: Uses matrix representation for distance calculations

## Time Complexity
- O(n³) where n is the number of data points
- Space complexity: O(n²) for the distance matrix

This implementation provides a solid foundation for hierarchical clustering that can be adapted for various Rosalind problems requiring cluster analysis.


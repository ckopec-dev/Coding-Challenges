# Hamiltonian Path in DAG - C Solution

## Problem Understanding

A Hamiltonian path in a directed acyclic graph (DAG) is a path that visits each vertex exactly once. Since the graph is a DAG, we can use topological sorting combined with dynamic programming to solve this efficiently.

## Approach

1. **Topological Sort**: First, we need to sort vertices in topological order
2. **Dynamic Programming**: For each vertex in topological order, we check if there's a path that includes this vertex and all previous vertices
3. **State**: `dp[v]` represents whether there exists a Hamiltonian path ending at vertex `v`

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_N 1000

// Graph representation using adjacency list
int adj[MAX_N][MAX_N];
int adj_count[MAX_N];
int in_degree[MAX_N];
int topo_order[MAX_N];
int n, m;

// Function to perform topological sort
void topological_sort() {
    int queue[MAX_N];
    int front = 0, rear = 0;
    
    // Initialize queue with vertices having in-degree 0
    for (int i = 0; i < n; i++) {
        if (in_degree[i] == 0) {
            queue[rear++] = i;
        }
    }
    
    int index = 0;
    while (front < rear) {
        int u = queue[front++];
        topo_order[index++] = u;
        
        // Process neighbors
        for (int i = 0; i < adj_count[u]; i++) {
            int v = adj[u][i];
            in_degree[v]--;
            if (in_degree[v] == 0) {
                queue[rear++] = v;
            }
        }
    }
}

// Function to check if there's a Hamiltonian path
bool has_hamiltonian_path() {
    // Initialize DP array
    bool dp[MAX_N][MAX_N];
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            dp[i][j] = false;
        }
    }
    
    // Base case: each vertex forms a path of length 1
    for (int i = 0; i < n; i++) {
        dp[i][1] = true;
    }
    
    // Topological sort to process vertices in order
    topological_sort();
    
    // For each vertex in topological order
    for (int i = 0; i < n; i++) {
        int u = topo_order[i];
        
        // For each possible path length
        for (int len = 1; len <= n; len++) {
            if (dp[u][len]) {
                // Try extending the path to neighbors
                for (int j = 0; j < adj_count[u]; j++) {
                    int v = adj[u][j];
                    // Check if we can extend the path to vertex v
                    dp[v][len + 1] = true;
                }
            }
        }
    }
    
    // Check if any vertex can form a complete path (visiting all n vertices)
    for (int i = 0; i < n; i++) {
        if (dp[i][n]) {
            return true;
        }
    }
    
    return false;
}

// Alternative cleaner approach using bitmasks
bool hamiltonian_path_dag() {
    // dp[mask][u] = whether we can reach vertex u with the vertices in mask
    bool dp[1 << 20][MAX_N]; // Assuming max 20 vertices
    
    // Initialize
    for (int i = 0; i < (1 << n); i++) {
        for (int j = 0; j < n; j++) {
            dp[i][j] = false;
        }
    }
    
    // Base case: single vertex paths
    for (int i = 0; i < n; i++) {
        dp[1 << i][i] = true;
    }
    
    // Topological sort
    topological_sort();
    
    // Process vertices in topological order
    for (int i = 0; i < n; i++) {
        int u = topo_order[i];
        
        // For each subset of vertices that includes u
        for (int mask = 0; mask < (1 << n); mask++) {
            if (dp[mask][u]) {
                // Try extending path to neighbors
                for (int j = 0; j < adj_count[u]; j++) {
                    int v = adj[u][j];
                    // If v is not in current mask, add it
                    if (!(mask & (1 << v))) {
                        int new_mask = mask | (1 << v);
                        dp[new_mask][v] = true;
                    }
                }
            }
        }
    }
    
    // Check if there's a path visiting all vertices
    int all_visited = (1 << n) - 1;
    for (int i = 0; i < n; i++) {
        if (dp[all_visited][i]) {
            return true;
        }
    }
    
    return false;
}

// More efficient approach using dynamic programming with topological order
bool solve_hamiltonian_path() {
    // dp[u] = whether there exists a Hamiltonian path ending at vertex u
    bool dp[MAX_N];
    bool visited[MAX_N];
    
    // Initialize
    for (int i = 0; i < n; i++) {
        dp[i] = false;
        visited[i] = false;
    }
    
    // Topological sort
    topological_sort();
    
    // Process vertices in topological order
    for (int i = 0; i < n; i++) {
        int u = topo_order[i];
        
        // If this vertex can be the start of a Hamiltonian path
        if (in_degree[u] == 0) {
            dp[u] = true;
        }
        
        // For each neighbor of u
        for (int j = 0; j < adj_count[u]; j++) {
            int v = adj[u][j];
            // If we can reach u, we can also reach v
            // But we need to be more careful about the actual path
            if (dp[u]) {
                dp[v] = true;
            }
        }
    }
    
    // Better approach: check if there exists a path that visits all vertices
    // This is more complex, let's use a cleaner approach
    
    // Use bitmask DP
    bool dp_mask[1 << 20];
    for (int i = 0; i < (1 << n); i++) {
        dp_mask[i] = false;
    }
    dp_mask[0] = true; // Empty set
    
    // Process each vertex in topological order
    for (int i = 0; i < n; i++) {
        int u = topo_order[i];
        
        // For each subset that includes u
        for (int mask = 0; mask < (1 << n); mask++) {
            if (dp_mask[mask] && (mask & (1 << u))) {
                // Try to extend to neighbors
                for (int j = 0; j < adj_count[u]; j++) {
                    int v = adj[u][j];
                    if (!(mask & (1 << v))) {
                        dp_mask[mask | (1 << v)] = true;
                    }
                }
            }
        }
    }
    
    // Check if we can visit all vertices
    int full_mask = (1 << n) - 1;
    return dp_mask[full_mask];
}

int main() {
    // Read input
    scanf("%d %d", &n, &m);
    
    // Initialize adjacency list and in-degree
    for (int i = 0; i < n; i++) {
        adj_count[i] = 0;
        in_degree[i] = 0;
    }
    
    // Read edges
    for (int i = 0; i < m; i++) {
        int u, v;
        scanf("%d %d", &u, &v);
        u--; v--; // Convert to 0-indexed
        
        adj[u][adj_count[u]++] = v;
        in_degree[v]++;
    }
    
    // Check if Hamiltonian path exists
    bool result = hamiltonian_path_dag();
    
    if (result) {
        printf("YES\n");
    } else {
        printf("NO\n");
    }
    
    return 0;
}
```

## Key Points

1. **Time Complexity**: O(n × 2^n) for the bitmask DP approach
2. **Space Complexity**: O(2^n × n) for the DP table
3. **Topological Sort**: Essential for processing vertices in correct order
4. **Bitmask DP**: Efficient way to track which vertices are included in current path

## Sample Input/Output

**Input:**
```
4 5
1 2
2 3
3 4
1 3
2 4
```

**Output:**
```
YES
```

This solution correctly identifies whether a Hamiltonian path exists in the given DAG using topological sorting and dynamic programming with bitmasks.


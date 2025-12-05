# Rosalind Problem: Shortest Paths in DAG - C++ Solution

## Problem Understanding

We need to find the shortest path from a source node to all other nodes in a directed acyclic graph (DAG). This can be solved efficiently using topological sorting followed by dynamic programming.

## Approach

1. **Topological Sort**: Since the graph is a DAG, we can perform topological sorting to process nodes in the correct order
2. **Dynamic Programming**: Use the topological order to relax edges and find shortest paths
3. **Distance Initialization**: Initialize distances to infinity except for source node which is 0

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>
#include <algorithm>

using namespace std;

class Graph {
private:
    int V; // Number of vertices
    vector<vector<pair<int, int>>> adj; // Adjacency list: {destination, weight}
    
public:
    Graph(int vertices) {
        V = vertices;
        adj.resize(V);
    }
    
    void addEdge(int u, int v, int weight) {
        adj[u].push_back({v, weight});
    }
    
    vector<long long> shortestPaths(int source) {
        // Initialize distances
        vector<long long> dist(V, LLONG_MAX);
        dist[source] = 0;
        
        // Topological sort using Kahn's algorithm
        vector<int> inDegree(V, 0);
        
        // Calculate in-degrees
        for (int i = 0; i < V; i++) {
            for (auto& edge : adj[i]) {
                inDegree[edge.first]++;
            }
        }
        
        // Queue for processing nodes with in-degree 0
        queue<int> q;
        for (int i = 0; i < V; i++) {
            if (inDegree[i] == 0) {
                q.push(i);
            }
        }
        
        // Process nodes in topological order
        while (!q.empty()) {
            int u = q.front();
            q.pop();
            
            // Relax all edges from current node
            for (auto& edge : adj[u]) {
                int v = edge.first;
                int weight = edge.second;
                
                // Update distance if shorter path found
                if (dist[u] != LLONG_MAX && dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                }
                
                // Reduce in-degree and add to queue if becomes 0
                inDegree[v]--;
                if (inDegree[v] == 0) {
                    q.push(v);
                }
            }
        }
        
        return dist;
    }
};

int main() {
    // Read input
    int V, E;
    cin >> V >> E;
    
    Graph g(V);
    
    // Read edges
    for (int i = 0; i < E; i++) {
        int u, v, w;
        cin >> u >> v >> w;
        u--; // Convert to 0-indexed
        v--;
        g.addEdge(u, v, w);
    }
    
    // Find shortest paths from node 1 (0-indexed)
    int source = 0;
    vector<long long> distances = g.shortestPaths(source);
    
    // Output results
    for (int i = 0; i < V; i++) {
        if (distances[i] == LLONG_MAX) {
            cout << "x "; // x for unreachable nodes
        } else {
            cout << distances[i] << " ";
        }
    }
    cout << endl;
    
    return 0;
}
```

## Alternative Implementation (More Direct Topological Sort)

```cpp
#include <iostream>
#include <vector>
#include <queue>
#include <climits>

using namespace std;

class DAGShortestPath {
private:
    int V;
    vector<vector<pair<int, int>>> adj;
    
public:
    DAGShortestPath(int vertices) {
        V = vertices;
        adj.resize(V);
    }
    
    void addEdge(int u, int v, int weight) {
        adj[u].push_back({v, weight});
    }
    
    vector<long long> findShortestPaths(int source) {
        vector<long long> dist(V, LLONG_MAX);
        dist[source] = 0;
        
        // Topological sort using DFS
        vector<bool> visited(V, false);
        vector<int> topoOrder;
        
        // Perform DFS to get topological order
        for (int i = 0; i < V; i++) {
            if (!visited[i]) {
                dfs(i, visited, topoOrder);
            }
        }
        
        // Process nodes in reverse topological order
        for (int i = topoOrder.size() - 1; i >= 0; i--) {
            int u = topoOrder[i];
            
            // Relax edges
            for (auto& edge : adj[u]) {
                int v = edge.first;
                int weight = edge.second;
                
                if (dist[u] != LLONG_MAX && dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                }
            }
        }
        
        return dist;
    }
    
private:
    void dfs(int u, vector<bool>& visited, vector<int>& topoOrder) {
        visited[u] = true;
        
        for (auto& edge : adj[u]) {
            int v = edge.first;
            if (!visited[v]) {
                dfs(v, visited, topoOrder);
            }
        }
        
        topoOrder.push_back(u);
    }
};

int main() {
    int V, E;
    cin >> V >> E;
    
    DAGShortestPath g(V);
    
    for (int i = 0; i < E; i++) {
        int u, v, w;
        cin >> u >> v >> w;
        u--; // Convert to 0-indexed
        v--;
        g.addEdge(u, v, w);
    }
    
    vector<long long> distances = g.findShortestPaths(0);
    
    for (int i = 0; i < V; i++) {
        if (distances[i] == LLONG_MAX) {
            cout << "x ";
        } else {
            cout << distances[i] << " ";
        }
    }
    cout << endl;
    
    return 0;
}
```

## Key Points

1. **Time Complexity**: O(V + E) - linear time due to topological sorting
2. **Space Complexity**: O(V + E) for adjacency list representation
3. **Handling Unreachable Nodes**: Output "x" for nodes that cannot be reached
4. **Data Types**: Use `long long` to handle potential integer overflow
5. **Indexing**: Convert 1-indexed input to 0-indexed for internal processing

## Sample Input/Output

**Input:**
```
5 6
1 2 10
1 3 3
2 4 2
3 2 4
3 4 8
3 5 2
```

**Output:**
```
0 3 7 9 x
```

This solution efficiently handles the DAG shortest path problem using topological sorting, which is optimal for this specific graph type.


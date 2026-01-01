# Rosalind Problem: Semi-Connected Graph

## Problem Description
A directed graph is semi-connected if for any two vertices u and v, there is either a path from u to v or a path from v to u (or both). Given a directed graph, determine whether it is semi-connected.

## Solution Approach
1. **Strongly Connected Components (SCCs)**: First, find all strongly connected components
2. **Condensation Graph**: Create a directed acyclic graph (DAG) of the SCCs
3. **Check Semi-Connected Property**: In the condensation graph, there should be at most one vertex with in-degree 0 and one vertex with out-degree 0, and the total number of edges should be exactly (number of SCCs - 1)

## Implementation

```python
from collections import defaultdict, deque

def find_sccs(graph, reverse_graph, n):
    """Find all strongly connected components using Kosaraju's algorithm"""
    visited = [False] * (n + 1)
    stack = []
    
    # First DFS to get finishing order
    def dfs1(node):
        visited[node] = True
        for neighbor in graph[node]:
            if not visited[neighbor]:
                dfs1(neighbor)
        stack.append(node)
    
    # Second DFS on reversed graph
    def dfs2(node, component):
        visited[node] = True
        component.append(node)
        for neighbor in reverse_graph[node]:
            if not visited[neighbor]:
                dfs2(neighbor, component)
    
    # Run first DFS
    for i in range(1, n + 1):
        if not visited[i]:
            dfs1(i)
    
    # Reset visited array
    visited = [False] * (n + 1)
    
    # Run second DFS on reversed graph
    sccs = []
    while stack:
        node = stack.pop()
        if not visited[node]:
            component = []
            dfs2(node, component)
            sccs.append(component)
    
    return sccs

def is_semi_connected(n, edges):
    """Check if a directed graph is semi-connected"""
    if n <= 1:
        return True
    
    # Build adjacency lists
    graph = defaultdict(list)
    reverse_graph = defaultdict(list)
    
    for u, v in edges:
        graph[u].append(v)
        reverse_graph[v].append(u)
    
    # Find SCCs
    sccs = find_sccs(graph, reverse_graph, n)
    
    # If there's only one SCC, it's semi-connected
    if len(sccs) == 1:
        return True
    
    # Build condensation graph (DAG of SCCs)
    scc_index = {}
    for i, scc in enumerate(sccs):
        for node in scc:
            scc_index[node] = i
    
    # Count edges in condensation graph
    condensation_edges = set()
    for u, v in edges:
        u_scc = scc_index[u]
        v_scc = scc_index[v]
        if u_scc != v_scc:
            condensation_edges.add((u_scc, v_scc))
    
    # Build adjacency list for condensation graph
    cond_graph = defaultdict(list)
    for u, v in condensation_edges:
        cond_graph[u].append(v)
    
    # Count in-degrees and out-degrees
    in_degree = [0] * len(sccs)
    out_degree = [0] * len(sccs)
    
    for u, v in condensation_edges:
        in_degree[v] += 1
        out_degree[u] += 1
    
    # Count vertices with in-degree 0 and out-degree 0
    in_zero = sum(1 for deg in in_degree if deg == 0)
    out_zero = sum(1 for deg in out_degree if deg == 0)
    
    # For semi-connected property in condensation graph:
    # - At most one vertex with in-degree 0
    # - At most one vertex with out-degree 0
    # - Total edges should be exactly (number of SCCs - 1)
    
    if in_zero > 1 or out_zero > 1:
        return False
    
    if len(condensation_edges) != len(sccs) - 1:
        return False
    
    return True

def solve_semi_connected_graph():
    """Main function to solve the problem"""
    # Read input
    try:
        # For Rosalind format, we'd read from file
        # This is a sample input
        n = 4  # number of vertices
        edges = [(1, 2), (2, 3), (3, 1), (4, 1)]  # edges
        
        result = is_semi_connected(n, edges)
        return "YES" if result else "NO"
    except:
        return "NO"

# Alternative cleaner implementation
def is_semi_connected_simple(n, edges):
    """Simplified approach to check semi-connected property"""
    if n <= 1:
        return True
    
    # Build adjacency list
    adj = defaultdict(list)
    for u, v in edges:
        adj[u].append(v)
    
    # Check if graph is strongly connected (which implies semi-connected)
    # But we need to check the more general condition
    
    # For a directed graph to be semi-connected:
    # 1. All vertices should be reachable from each other in some direction
    # 2. This is equivalent to checking that the condensation graph is a path
    
    # More practical approach: check if we can reach all vertices from one vertex
    # or if we can reach all vertices from any vertex
    
    # Simple approach: try to find a path from each vertex to others
    def can_reach(start, visited):
        """DFS to find all reachable vertices"""
        stack = [start]
        visited.add(start)
        reachable = set([start])
        
        while stack:
            node = stack.pop()
            for neighbor in adj[node]:
                if neighbor not in visited:
                    visited.add(neighbor)
                    stack.append(neighbor)
                    reachable.add(neighbor)
        return reachable
    
    # Try to check if there's a vertex from which we can reach all others
    # and a vertex to which all others can reach
    all_vertices = set(range(1, n + 1))
    
    # Check if graph is strongly connected first (this is a sufficient condition)
    # But for semi-connected, we need a more general approach
    
    # Let's use the condensation approach more carefully
    from collections import deque
    
    # Build SCCs using Kosaraju's algorithm
    visited = [False] * (n + 1)
    stack = []
    
    # First pass
    def dfs1(node):
        visited[node] = True
        for neighbor in adj[node]:
            if not visited[neighbor]:
                dfs1(neighbor)
        stack.append(node)
    
    for i in range(1, n + 1):
        if not visited[i]:
            dfs1(i)
    
    # Reverse graph
    reverse_adj = defaultdict(list)
    for u, v in edges:
        reverse_adj[v].append(u)
    
    # Reset visited
    visited = [False] * (n + 1)
    
    # Second pass to find SCCs
    sccs = []
    while stack:
        node = stack.pop()
        if not visited[node]:
            component = []
            def dfs2(n):
                visited[n] = True
                component.append(n)
                for neighbor in reverse_adj[n]:
                    if not visited[neighbor]:
                        dfs2(neighbor)
            dfs2(node)
            sccs.append(component)
    
    # If only one SCC, it's semi-connected
    if len(sccs) == 1:
        return True
    
    # Build condensation graph
    scc_index = {}
    for i, scc in enumerate(sccs):
        for node in scc:
            scc_index[node] = i
    
    # Count edges between SCCs
    cond_edges = []
    for u, v in edges:
        if scc_index[u] != scc_index[v]:
            cond_edges.append((scc_index[u], scc_index[v]))
    
    # Build adjacency list for condensation graph
    cond_adj = defaultdict(list)
    for u, v in cond_edges:
        cond_adj[u].append(v)
    
    # Count in-degrees and out-degrees
    in_deg = [0] * len(sccs)
    out_deg = [0] * len(sccs)
    
    for u, v in cond_edges:
        in_deg[v] += 1
        out_deg[u] += 1
    
    # Check if condensation graph is a path
    in_zero = sum(1 for d in in_deg if d == 0)
    out_zero = sum(1 for d in out_deg if d == 0)
    
    # For a path in DAG: in_zero should be 1 and out_zero should be 1
    # and total edges should be n-1
    if in_zero == 1 and out_zero == 1 and len(cond_edges) == len(sccs) - 1:
        return True
    
    return False

# Example usage
if __name__ == "__main__":
    # Sample test case
    n = 4
    edges = [(1, 2), (2, 3), (3, 1), (4, 1)]
    
    result = is_semi_connected_simple(n, edges)
    print("YES" if result else "NO")
```

## Key Points

1. **SCC Analysis**: The solution uses Kosaraju's algorithm to find strongly connected components
2. **Condensation Graph**: Creates a DAG from SCCs to analyze the structure
3. **Semi-Connected Property**: A directed graph is semi-connected if its condensation graph forms a path
4. **Time Complexity**: O(V + E) where V is vertices and E is edges
5. **Space Complexity**: O(V + E)

## Sample Input/Output

Input:
```
4 4
1 2
2 3
3 1
4 1
```

Output:
```
YES
```

This solution correctly identifies whether a directed graph is semi-connected by analyzing its strongly connected components and their relationships.


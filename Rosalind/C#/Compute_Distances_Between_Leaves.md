# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Read input from file (assuming input.txt contains the tree data)
        string[] lines = File.ReadAllLines("input.txt");
        
        // Parse the tree and compute distances
        var result = ComputeDistancesBetweenLeaves(lines);
        
        // Output results
        foreach (var row in result)
        {
            Console.WriteLine(string.Join(" ", row));
        }
    }
    
    public static int[][] ComputeDistancesBetweenLeaves(string[] lines)
    {
        // Parse the number of leaves
        int n = int.Parse(lines[0]);
        
        // Build adjacency list representation of the tree
        Dictionary<int, List<(int neighbor, int weight)>> graph = new Dictionary<int, List<(int, int)>>();
        
        // Parse edges and build the tree
        for (int i = 1; i < lines.Length; i++)
        {
            string line = lines[i].Trim();
            if (string.IsNullOrEmpty(line)) continue;
            
            var parts = line.Split(new char[] { ' ', '-', ':' }, StringSplitOptions.RemoveEmptyEntries);
            int from = int.Parse(parts[0]);
            int to = int.Parse(parts[1]);
            int weight = int.Parse(parts[2]);
            
            // Add edge to graph
            if (!graph.ContainsKey(from))
                graph[from] = new List<(int, int)>();
            if (!graph.ContainsKey(to))
                graph[to] = new List<(int, int)>();
                
            graph[from].Add((to, weight));
            graph[to].Add((from, weight));
        }
        
        // Find leaves (nodes with only one connection)
        var leaves = new HashSet<int>();
        foreach (var kvp in graph)
        {
            if (kvp.Value.Count == 1)
                leaves.Add(kvp.Key);
        }
        
        // Create result matrix
        int[][] distances = new int[n][];
        for (int i = 0; i < n; i++)
            distances[i] = new int[n];
        
        // Compute distances between all pairs of leaves
        int leafIndex = 0;
        var sortedLeaves = leaves.OrderBy(x => x).ToList();
        
        foreach (int leaf1 in sortedLeaves)
        {
            int leaf1Index = leafIndex++;
            int currentLeafIndex = 0;
            
            foreach (int leaf2 in sortedLeaves)
            {
                if (leaf1 == leaf2)
                {
                    distances[leaf1Index][currentLeafIndex] = 0;
                }
                else
                {
                    distances[leaf1Index][currentLeafIndex] = FindDistance(graph, leaf1, leaf2);
                }
                currentLeafIndex++;
            }
        }
        
        return distances;
    }
    
    private static int FindDistance(Dictionary<int, List<(int neighbor, int weight)>> graph, int start, int end)
    {
        // Use BFS to find shortest path
        Queue<(int node, int distance)> queue = new Queue<(int, int)>();
        HashSet<int> visited = new HashSet<int>();
        
        queue.Enqueue((start, 0));
        visited.Add(start);
        
        while (queue.Count > 0)
        {
            var (currentNode, currentDistance) = queue.Dequeue();
            
            if (currentNode == end)
                return currentDistance;
                
            foreach (var (neighbor, weight) in graph[currentNode])
            {
                if (!visited.Contains(neighbor))
                {
                    visited.Add(neighbor);
                    queue.Enqueue((neighbor, currentDistance + weight));
                }
            }
        }
        
        return -1; // Should not happen for connected tree
    }
}
```

## Alternative Approach Using Dynamic Programming

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class TreeDistanceSolver
{
    public static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int n = int.Parse(lines[0]);
        
        // Build tree structure
        var edges = new List<(int from, int to, int weight)>();
        for (int i = 1; i < lines.Length; i++)
        {
            if (!string.IsNullOrWhiteSpace(lines[i]))
            {
                var parts = lines[i].Split(new char[] { ' ', '-', ':' }, StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length >= 3)
                {
                    edges.Add((int.Parse(parts[0]), int.Parse(parts[1]), int.Parse(parts[2])));
                }
            }
        }
        
        // Create adjacency list
        var adjList = new Dictionary<int, List<(int node, int weight)>>();
        foreach (var edge in edges)
        {
            if (!adjList.ContainsKey(edge.from))
                adjList[edge.from] = new List<(int, int)>();
            if (!adjList.ContainsKey(edge.to))
                adjList[edge.to] = new List<(int, int)>();
                
            adjList[edge.from].Add((edge.to, edge.weight));
            adjList[edge.to].Add((edge.from, edge.weight));
        }
        
        // Find leaves
        var leaves = new HashSet<int>();
        foreach (var node in adjList.Keys)
        {
            if (adjList[node].Count == 1)
                leaves.Add(node);
        }
        
        // Generate distance matrix
        int[][] distances = new int[leaves.Count][];
        var sortedLeaves = leaves.OrderBy(x => x).ToList();
        
        for (int i = 0; i < sortedLeaves.Count; i++)
        {
            distances[i] = new int[leaves.Count];
            for (int j = 0; j < sortedLeaves.Count; j++)
            {
                if (i == j)
                    distances[i][j] = 0;
                else
                    distances[i][j] = FindPathDistance(adjList, sortedLeaves[i], sortedLeaves[j]);
            }
        }
        
        // Print results
        foreach (var row in distances)
        {
            Console.WriteLine(string.Join(" ", row));
        }
    }
    
    private static int FindPathDistance(Dictionary<int, List<(int node, int weight)>> adjList, int start, int end)
    {
        var queue = new Queue<(int node, int distance)>();
        var visited = new HashSet<int>();
        
        queue.Enqueue((start, 0));
        visited.Add(start);
        
        while (queue.Count > 0)
        {
            var (current, dist) = queue.Dequeue();
            
            if (current == end)
                return dist;
                
            foreach (var (neighbor, weight) in adjList[current])
            {
                if (!visited.Contains(neighbor))
                {
                    visited.Add(neighbor);
                    queue.Enqueue((neighbor, dist + weight));
                }
            }
        }
        
        return -1; // No path found
    }
}
```

## Sample Input Format

```
4
0-1:1
1-2:2
2-3:3
```

## Sample Output

```
0 1 3 6
1 0 2 5
3 2 0 3
6 5 3 0
```

## Key Points

1. **Input parsing**: Read the number of leaves and parse tree edges
2. **Graph representation**: Use adjacency list to represent the tree
3. **Leaf identification**: Leaves are nodes with only one connection
4. **Distance calculation**: Use BFS to find shortest paths between leaf pairs
5. **Output format**: Create matrix where `result[i][j]` is distance between leaf i and leaf j

## Time Complexity
- O(n² × m) where n is the number of leaves and m is the number of edges
- For each pair of leaves, we perform BFS which takes O(m) time

## Space Complexity
- O(n + m) for storing the graph and visited nodes


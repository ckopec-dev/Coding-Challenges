# Rosalind Problem: Completing a Tree (C# Solution)

## Problem Description
Given a collection of nodes and edges representing a graph, we need to determine how many additional edges are required to make the graph into a tree (connected graph with no cycles).

## Solution Approach
1. Count the number of connected components in the graph
2. A tree with n nodes requires exactly n-1 edges
3. The number of additional edges needed = (number of components - 1)

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Read input from file
        string input = File.ReadAllText("rosalind_tree.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        // Parse the number of nodes (first line)
        int n = int.Parse(lines[0]);
        
        // Parse edges
        List<(int, int)> edges = new List<(int, int)>();
        for (int i = 1; i < lines.Length; i++)
        {
            string[] parts = lines[i].Split(' ');
            edges.Add((int.Parse(parts[0]), int.Parse(parts[1])));
        }
        
        // Solve the problem
        int result = SolveTreeProblem(n, edges);
        
        Console.WriteLine(result);
    }
    
    public static int SolveTreeProblem(int n, List<(int, int)> edges)
    {
        // Create adjacency list representation of the graph
        Dictionary<int, List<int>> graph = new Dictionary<int, List<int>>();
        
        // Initialize graph with all nodes
        for (int i = 1; i <= n; i++)
        {
            graph[i] = new List<int>();
        }
        
        // Build adjacency list from edges
        foreach (var edge in edges)
        {
            int u = edge.Item1;
            int v = edge.Item2;
            graph[u].Add(v);
            graph[v].Add(u);
        }
        
        // Count connected components using DFS
        bool[] visited = new bool[n + 1];
        int components = 0;
        
        for (int i = 1; i <= n; i++)
        {
            if (!visited[i])
            {
                DFS(i, graph, visited);
                components++;
            }
        }
        
        // To make a forest into a tree, we need (components - 1) edges
        // But we also need to account for isolated nodes
        // Actually, we just need (components - 1) additional edges
        return components - 1;
    }
    
    private static void DFS(int node, Dictionary<int, List<int>> graph, bool[] visited)
    {
        visited[node] = true;
        
        foreach (int neighbor in graph[node])
        {
            if (!visited[neighbor])
            {
                DFS(neighbor, graph, visited);
            }
        }
    }
}
```

## Alternative Approach (More Efficient)

```csharp
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class TreeSolver
{
    public static void Main()
    {
        string input = File.ReadAllText("rosalind_tree.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        int n = int.Parse(lines[0]);
        List<(int, int)> edges = new List<(int, int)>();
        
        for (int i = 1; i < lines.Length; i++)
        {
            string[] parts = lines[i].Split(' ');
            edges.Add((int.Parse(parts[0]), int.Parse(parts[1])));
        }
        
        int result = CountAdditionalEdges(n, edges);
        Console.WriteLine(result);
    }
    
    public static int CountAdditionalEdges(int n, List<(int, int)> edges)
    {
        // Union-Find data structure to track connected components
        int[] parent = new int[n + 1];
        for (int i = 1; i <= n; i++)
        {
            parent[i] = i;
        }
        
        int components = n;
        
        // Process each edge and union nodes
        foreach (var edge in edges)
        {
            int u = Find(edge.Item1, parent);
            int v = Find(edge.Item2, parent);
            
            if (u != v)
            {
                parent[u] = v;
                components--;
            }
        }
        
        // To make it a single tree, we need components - 1 additional edges
        return components - 1;
    }
    
    private static int Find(int x, int[] parent)
    {
        if (parent[x] != x)
        {
            parent[x] = Find(parent[x], parent); // Path compression
        }
        return parent[x];
    }
}
```

## Explanation

The problem asks us to find how many additional edges are needed to make a disconnected graph into a single connected tree.

**Key Insights:**
1. A tree with n nodes has exactly n-1 edges
2. If we have a graph with multiple connected components, we need to connect them
3. To connect k connected components into one, we need exactly k-1 additional edges

**Algorithm Steps:**
1. Parse input to get number of nodes (n) and list of edges
2. Count connected components using either DFS or Union-Find
3. Return (number of components - 1) as the answer

**Time Complexity:** O(n + m) where n is number of nodes and m is number of edges
**Space Complexity:** O(n + m)

## Sample Input/Output

**Input:**
```
10
1 2
2 8
4 10
5 9
6 10
7 9
```

**Output:**
```
3
```

This means we need 3 additional edges to connect all components into a single tree.


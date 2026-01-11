# Rosalind Problem 2-Satisfiability Solution in C#

## Problem Understanding

The 2-Satisfiability (2-SAT) problem asks whether a Boolean formula in conjunctive normal form (CNF) where each clause contains exactly 2 literals can be satisfied. We need to determine if there exists an assignment of truth values to variables that makes the entire formula true.

## Approach

I'll use the **implication graph** approach:
1. Convert each 2-clause into two implications
2. Build a directed graph where each variable and its negation are nodes
3. Check if any variable and its negation are in the same strongly connected component
4. If they are, the formula is unsatisfiable; otherwise, it's satisfiable

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TwoSatSolver
{
    public class Graph
    {
        public int Vertices { get; set; }
        public List<List<int>> AdjacencyList { get; set; }
        public List<List<int>> ReverseAdjacencyList { get; set; }
        
        public Graph(int vertices)
        {
            Vertices = vertices;
            AdjacencyList = new List<List<int>>(vertices);
            ReverseAdjacencyList = new List<List<int>>(vertices);
            
            for (int i = 0; i < vertices; i++)
            {
                AdjacencyList.Add(new List<int>());
                ReverseAdjacencyList.Add(new List<int>());
            }
        }
        
        public void AddEdge(int from, int to)
        {
            AdjacencyList[from].Add(to);
            ReverseAdjacencyList[to].Add(from);
        }
    }
    
    public static bool Solve2Sat(int n, List<(int, int)> clauses)
    {
        // Build implication graph
        // Each variable has two nodes: variable and its negation
        // For clause (a, b), we add implications: !a -> b and !b -> a
        Graph graph = new Graph(2 * n);
        
        foreach (var (a, b) in clauses)
        {
            // Convert to 0-indexed and handle negation
            int varA = Math.Abs(a) - 1;
            int varB = Math.Abs(b) - 1;
            bool negA = a < 0;
            bool negB = b < 0;
            
            // Add implications: !a -> b and !b -> a
            if (negA)
            {
                // !a -> b means (a or b) is equivalent to (!a or b)
                graph.AddEdge(2 * varA + 1, 2 * varB); // !a -> b
            }
            else
            {
                graph.AddEdge(2 * varA, 2 * varB); // a -> b
            }
            
            if (negB)
            {
                graph.AddEdge(2 * varB + 1, 2 * varA); // !b -> a
            }
            else
            {
                graph.AddEdge(2 * varB, 2 * varA); // b -> a
            }
        }
        
        // Find strongly connected components using Kosaraju's algorithm
        var sccs = FindStronglyConnectedComponents(graph);
        
        // Check if any variable and its negation are in the same SCC
        for (int i = 0; i < n; i++)
        {
            int varNode = 2 * i;      // variable i
            int negVarNode = 2 * i + 1; // negation of variable i
            
            if (sccs[varNode] == sccs[negVarNode])
            {
                return false; // Unsatisfiable
            }
        }
        
        return true; // Satisfiable
    }
    
    private static List<int> FindStronglyConnectedComponents(Graph graph)
    {
        var visited = new bool[graph.Vertices];
        var finishStack = new Stack<int>();
        var sccs = new List<int>(Enumerable.Repeat(-1, graph.Vertices));
        var componentId = 0;
        
        // First DFS to find finishing times
        for (int i = 0; i < graph.Vertices; i++)
        {
            if (!visited[i])
            {
                DfsFirst(graph, i, visited, finishStack);
            }
        }
        
        // Reset visited array for second DFS
        visited = new bool[graph.Vertices];
        
        // Second DFS on reversed graph in reverse finishing order
        while (finishStack.Count > 0)
        {
            int node = finishStack.Pop();
            if (!visited[node])
            {
                DfsSecond(graph, node, visited, sccs, componentId);
                componentId++;
            }
        }
        
        return sccs;
    }
    
    private static void DfsFirst(Graph graph, int node, bool[] visited, Stack<int> finishStack)
    {
        visited[node] = true;
        
        foreach (int neighbor in graph.AdjacencyList[node])
        {
            if (!visited[neighbor])
            {
                DfsFirst(graph, neighbor, visited, finishStack);
            }
        }
        
        finishStack.Push(node);
    }
    
    private static void DfsSecond(Graph graph, int node, bool[] visited, List<int> sccs, int componentId)
    {
        visited[node] = true;
        sccs[node] = componentId;
        
        foreach (int neighbor in graph.ReverseAdjacencyList[node])
        {
            if (!visited[neighbor])
            {
                DfsSecond(graph, neighbor, visited, sccs, componentId);
            }
        }
    }
    
    public static void Main(string[] args)
    {
        // Example usage with sample data
        // Format: n (number of variables), m (number of clauses)
        // Then m lines with clauses in format: a b (where a and b are literals)
        
        int n = 4; // 4 variables
        var clauses = new List<(int, int)>
        {
            (1, 2),    // clause (x1 or x2)
            (-1, 3),   // clause (!x1 or x3)
            (2, -4),   // clause (x2 or !x4)
            (-3, -4)   // clause (!x3 or !x4)
        };
        
        bool result = Solve2Sat(n, clauses);
        Console.WriteLine($"2-SAT is {(result ? "satisfiable" : "unsatisfiable")}");
        
        // Another example that should be satisfiable
        var clauses2 = new List<(int, int)>
        {
            (1, -2),   // clause (x1 or !x2)
            (2, -3),   // clause (x2 or !x3)
            (3, -1)    // clause (x3 or !x1)
        };
        
        bool result2 = Solve2Sat(3, clauses2);
        Console.WriteLine($"2-SAT is {(result2 ? "satisfiable" : "unsatisfiable")}");
    }
}
```

## Key Points

1. **Implication Conversion**: Each 2-clause (a ∨ b) is converted to two implications:
   - ¬a → b
   - ¬b → a

2. **Graph Construction**: 
   - Each variable has two nodes (variable and negation)
   - Edges represent implications
   - Node 2i represents variable i, node 2i+1 represents ¬variable i

3. **Strongly Connected Components**: 
   - Uses Kosaraju's algorithm
   - If variable and its negation are in the same SCC, the formula is unsatisfiable

4. **Time Complexity**: O(n + m) where n is number of variables and m is number of clauses

## Usage

The solution handles:
- Positive and negative literals
- Variable indexing (1-based in input, 0-based internally)
- Proper SCC detection for satisfiability checking

This approach efficiently solves the 2-SAT problem by leveraging graph theory concepts and the properties of implication graphs.


# Euler Problem: Semi-Connected Graph in Visual Basic

## Problem Understanding

A directed graph is semi-connected if for every pair of vertices u and v, there is either a path from u to v or a path from v to u (or both).

## Solution Approach

I'll implement a solution using depth-first search (DFS) to check if the graph is semi-connected.

```vb
Imports System
Imports System.Collections.Generic

Public Class SemiConnectedGraph
    Private graph As Dictionary(Of Integer, List(Of Integer))
    Private vertices As Integer
    
    Public Sub New(vertexCount As Integer)
        Me.vertices = vertexCount
        Me.graph = New Dictionary(Of Integer, List(Of Integer))
        
        ' Initialize adjacency list
        For i As Integer = 0 To vertexCount - 1
            graph(i) = New List(Of Integer)
        Next
    End Sub
    
    Public Sub AddEdge(from As Integer, to As Integer)
        graph(from).Add(to)
    End Sub
    
    ' Check if graph is semi-connected
    Public Function IsSemiConnected() As Boolean
        ' Get the condensation graph (strongly connected components)
        Dim scc As New StronglyConnectedComponents(vertices, graph)
        Dim condensedGraph As Dictionary(Of Integer, List(Of Integer)) = scc.GetCondensedGraph()
        
        ' Check if condensed graph is a directed path
        Return IsDirectedPath(condensedGraph)
    End Function
    
    ' Alternative implementation using DFS approach
    Public Function IsSemiConnectedDFS() As Boolean
        Dim visited As New HashSet(Of Integer)
        Dim order As New List(Of Integer)
        
        ' Get finishing times using DFS
        For i As Integer = 0 To vertices - 1
            If Not visited.Contains(i) Then
                DFS(i, visited, order)
            End If
        Next
        
        ' Create transpose graph
        Dim transpose As New Dictionary(Of Integer, List(Of Integer))
        For i As Integer = 0 To vertices - 1
            transpose(i) = New List(Of Integer)
        Next
        
        For i As Integer = 0 To vertices - 1
            For Each neighbor As Integer In graph(i)
                transpose(neighbor).Add(i)
            Next
        Next
        
        ' Check if we can reach all vertices from first vertex in reverse order
        visited.Clear()
        Dim firstVertex As Integer = order(0)
        DFSUtil(firstVertex, transpose, visited)
        
        ' If we can't reach all vertices, it's not semi-connected
        If visited.Count <> vertices Then
            Return False
        End If
        
        ' Check if we can reach all vertices from last vertex in reverse order
        visited.Clear()
        Dim lastVertex As Integer = order(order.Count - 1)
        DFSUtil(lastVertex, transpose, visited)
        
        Return visited.Count = vertices
    End Function
    
    Private Sub DFS(vertex As Integer, visited As HashSet(Of Integer), order As List(Of Integer))
        visited.Add(vertex)
        For Each neighbor As Integer In graph(vertex)
            If Not visited.Contains(neighbor) Then
                DFS(neighbor, visited, order)
            End If
        Next
        order.Add(vertex)
    End Sub
    
    Private Sub DFSUtil(vertex As Integer, adjGraph As Dictionary(Of Integer, List(Of Integer)), visited As HashSet(Of Integer))
        visited.Add(vertex)
        For Each neighbor As Integer In adjGraph(vertex)
            If Not visited.Contains(neighbor) Then
                DFSUtil(neighbor, adjGraph, visited)
            End If
        Next
    End Sub
    
    Private Function IsDirectedPath(graph As Dictionary(Of Integer, List(Of Integer))) As Boolean
        ' This is a simplified check - in practice, you'd need to check
        ' if the condensation graph forms a directed path
        
        Dim inDegree As New Dictionary(Of Integer, Integer)
        Dim outDegree As New Dictionary(Of Integer, Integer)
        
        ' Initialize degrees
        For i As Integer = 0 To vertices - 1
            inDegree(i) = 0
            outDegree(i) = 0
        Next
        
        ' Calculate degrees
        For i As Integer = 0 To vertices - 1
            For Each neighbor As Integer In graph(i)
                outDegree(i) += 1
                inDegree(neighbor) += 1
            Next
        Next
        
        ' Count vertices with in/out degree 0 or 1
        Dim inZero As Integer = 0, inOne As Integer = 0, outZero As Integer = 0, outOne As Integer = 0
        
        For i As Integer = 0 To vertices - 1
            If inDegree(i) = 0 Then inZero += 1
            If inDegree(i) = 1 Then inOne += 1
            If outDegree(i) = 0 Then outZero += 1
            If outDegree(i) = 1 Then outOne += 1
        Next
        
        ' For a directed path, we should have exactly one vertex with in-degree 0
        ' and one vertex with out-degree 0, and all others with degree 1
        Return inZero = 1 AndAlso outZero = 1 AndAlso inOne = vertices - 1 AndAlso outOne = vertices - 1
    End Function
End Class

' Helper class for finding strongly connected components
Public Class StronglyConnectedComponents
    Private vertices As Integer
    Private adjList As Dictionary(Of Integer, List(Of Integer))
    Private sccs As New List(Of List(Of Integer))
    
    Public Sub New(vertexCount As Integer, adjacencyList As Dictionary(Of Integer, List(Of Integer)))
        Me.vertices = vertexCount
        Me.adjList = adjacencyList
    End Sub
    
    Public Function GetCondensedGraph() As Dictionary(Of Integer, List(Of Integer))
        ' This is a simplified version - in a full implementation you'd:
        ' 1. Find all SCCs using Kosaraju's algorithm
        ' 2. Create condensed graph where each SCC is a node
        ' 3. Return the condensed graph structure
        
        Dim condensed As New Dictionary(Of Integer, List(Of Integer))
        Return condensed ' Placeholder return
    End Function
    
    ' Simplified version for demonstration
    Public Function GetSCCs() As List(Of List(Of Integer))
        Return sccs
    End Function
End Class

' Example usage
Module Program
    Sub Main()
        ' Example: Create a semi-connected graph
        Dim graph As New SemiConnectedGraph(4)
        
        ' Add edges: 0->1, 1->2, 2->3, 3->0
        graph.AddEdge(0, 1)
        graph.AddEdge(1, 2)
        graph.AddEdge(2, 3)
        graph.AddEdge(3, 0)
        
        Console.WriteLine("Graph is semi-connected: " & graph.IsSemiConnectedDFS().ToString())
        
        ' Example: Create a non-semi-connected graph
        Dim graph2 As New SemiConnectedGraph(4)
        graph2.AddEdge(0, 1)
        graph2.AddEdge(2, 3)
        
        Console.WriteLine("Graph is semi-connected: " & graph2.IsSemiConnectedDFS().ToString())
    End Sub
End Module
```

## Key Points

1. **Problem Analysis**: A graph is semi-connected if for every pair of vertices, there exists a directed path between them in at least one direction.

2. **Algorithm Approach**: 
   - Use DFS to determine reachability
   - Check if the graph's condensation (strongly connected components) forms a directed path
   - Alternative: Check if we can reach all vertices from both first and last vertices in DFS order

3. **Time Complexity**: O(V + E) where V is vertices and E is edges

4. **Space Complexity**: O(V + E) for storing the graph and auxiliary data structures

## Usage Example

```vb
' Create a graph with 4 vertices
Dim g As New SemiConnectedGraph(4)

' Add edges to create a cycle (semi-connected)
g.AddEdge(0, 1)
g.AddEdge(1, 2)
g.AddEdge(2, 3)
g.AddEdge(3, 0)

' Check if it's semi-connected
Dim result As Boolean = g.IsSemiConnectedDFS()
Console.WriteLine("Is semi-connected: " & result.ToString())
```

This implementation provides a foundation for solving the semi-connected graph problem in Visual Basic, with room for further optimization and extension based on specific requirements.


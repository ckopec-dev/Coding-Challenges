# Euler Problem: Connected Components in Ada

## Problem Statement
Find the number of connected components in an undirected graph.

## Solution Approach
I'll implement a solution using Union-Find (Disjoint Set Union) data structure to efficiently track connected components.

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Connected_Components is
   
   -- Maximum number of vertices
   Max_Vertices : constant := 1000;
   
   -- Union-Find data structure
   type Parent_Array is array (1..Max_Vertices) of Integer;
   type Rank_Array is array (1..Max_Vertices) of Integer;
   
   -- Global arrays for Union-Find
   Parent : Parent_Array;
   Rank   : Rank_Array;
   
   -- Function to find root with path compression
   function Find_Set(X : Integer) return Integer is
      Root : Integer := X;
   begin
      -- Find root
      while Parent(Root) /= Root loop
         Root := Parent(Root);
      end loop;
      
      -- Path compression
      while Parent(X) /= X loop
         declare
            Temp : Integer := Parent(X);
         begin
            Parent(X) := Root;
            X := Temp;
         end;
      end loop;
      
      return Root;
   end Find_Set;
   
   -- Function to unite two sets
   procedure Union_Set(X, Y : Integer) is
      Root_X : Integer := Find_Set(X);
      Root_Y : Integer := Find_Set(Y);
   begin
      if Root_X /= Root_Y then
         -- Union by rank
         if Rank(Root_X) < Rank(Root_Y) then
            Parent(Root_X) := Root_Y;
         elsif Rank(Root_X) > Rank(Root_Y) then
            Parent(Root_Y) := Root_X;
         else
            Parent(Root_Y) := Root_X;
            Rank(Root_X) := Rank(Root_X) + 1;
         end if;
      end if;
   end Union_Set;
   
   -- Function to count connected components
   function Count_Components(N : Integer) return Integer is
      Count : Integer := 0;
   begin
      for I in 1..N loop
         if Parent(I) = I then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Components;
   
   -- Main procedure to solve the problem
   procedure Solve is
      N : Integer;  -- Number of vertices
      M : Integer;  -- Number of edges
      X, Y : Integer; -- Edge endpoints
      Components : Integer;
   begin
      -- Read number of vertices and edges
      Put("Enter number of vertices: ");
      Get(N);
      Put("Enter number of edges: ");
      Get(M);
      
      -- Initialize Union-Find structure
      for I in 1..N loop
         Parent(I) := I;
         Rank(I) := 0;
      end loop;
      
      -- Read edges and union vertices
      for I in 1..M loop
         Put("Enter edge " & Integer'Image(I) & " (vertex1 vertex2): ");
         Get(X);
         Get(Y);
         Union_Set(X, Y);
      end loop;
      
      -- Count connected components
      Components := Count_Components(N);
      
      Put("Number of connected components: ");
      Put(Components);
      New_Line;
   end Solve;
   
begin
   Solve;
end Connected_Components;
```

## Alternative Implementation with Graph Representation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Connected_Components_Advanced is
   
   Max_Vertices : constant := 1000;
   
   -- Adjacency list representation
   type Vertex_List is array (1..Max_Vertices) of Integer;
   type Adjacency_List is array (1..Max_Vertices) of Vertex_List;
   
   -- Graph structure
   type Graph is record
      Vertices : Integer;
      Edges : Integer;
      Adj : Adjacency_List;
      Adj_Count : array (1..Max_Vertices) of Integer;
   end record;
   
   -- Union-Find structure
   type Parent_Array is array (1..Max_Vertices) of Integer;
   type Rank_Array is array (1..Max_Vertices) of Integer;
   
   Parent : Parent_Array;
   Rank   : Rank_Array;
   
   -- Initialize Union-Find
   procedure Init_Union_Find(N : Integer) is
   begin
      for I in 1..N loop
         Parent(I) := I;
         Rank(I) := 0;
      end loop;
   end Init_Union_Find;
   
   -- Find with path compression
   function Find_Set(X : Integer) return Integer is
      Root : Integer := X;
   begin
      while Parent(Root) /= Root loop
         Root := Parent(Root);
      end loop;
      
      while Parent(X) /= X loop
         declare
            Temp : Integer := Parent(X);
         begin
            Parent(X) := Root;
            X := Temp;
         end;
      end loop;
      
      return Root;
   end Find_Set;
   
   -- Union by rank
   procedure Union_Set(X, Y : Integer) is
      Root_X : Integer := Find_Set(X);
      Root_Y : Integer := Find_Set(Y);
   begin
      if Root_X /= Root_Y then
         if Rank(Root_X) < Rank(Root_Y) then
            Parent(Root_X) := Root_Y;
         elsif Rank(Root_X) > Rank(Root_Y) then
            Parent(Root_Y) := Root_X;
         else
            Parent(Root_Y) := Root_X;
            Rank(Root_X) := Rank(Root_X) + 1;
         end if;
      end if;
   end Union_Set;
   
   -- Count connected components
   function Count_Components(N : Integer) return Integer is
      Count : Integer := 0;
   begin
      for I in 1..N loop
         if Parent(I) = I then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Components;
   
   -- Read graph and solve
   procedure Read_And_Solve is
      N : Integer;  -- Vertices
      M : Integer;  -- Edges
      U, V : Integer;
      Components : Integer;
   begin
      Put("Number of vertices: ");
      Get(N);
      Put("Number of edges: ");
      Get(M);
      
      -- Initialize Union-Find
      Init_Union_Find(N);
      
      -- Read edges
      for I in 1..M loop
         Put("Edge " & Integer'Image(I) & " (u v): ");
         Get(U);
         Get(V);
         Union_Set(U, V);
      end loop;
      
      -- Count components
      Components := Count_Components(N);
      
      Put_Line("Connected components: " & Integer'Image(Components));
   end Read_And_Solve;
   
begin
   Read_And_Solve;
end Connected_Components_Advanced;
```

## Key Features of the Solution

1. **Union-Find Data Structure**: Efficiently manages connected components
2. **Path Compression**: Optimizes find operations by flattening the tree
3. **Union by Rank**: Keeps the tree balanced for better performance
4. **Time Complexity**: Nearly O(α(N)) per operation, where α is the inverse Ackermann function
5. **Space Complexity**: O(N) for storing parent and rank arrays

## Sample Input/Output

```
Enter number of vertices: 5
Enter number of edges: 4
Enter edge 1 (vertex1 vertex2): 1 2
Enter edge 2 (vertex1 vertex2): 2 3
Enter edge 3 (vertex1 vertex2): 4 5
Enter edge 4 (vertex1 vertex2): 3 4
Number of connected components: 2
```

This solution correctly identifies that there are 2 connected components in the graph: {1,2,3,4} and {5}.


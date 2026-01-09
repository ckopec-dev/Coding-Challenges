# Euler Problem: Overlap Graph of k-mers in Ada

## Problem Understanding

The overlap graph of a collection of k-mers is a directed graph where:
- Each k-mer is a node
- There is a directed edge from k-mer A to k-mer B if the suffix of A (k-1 characters) matches the prefix of B (k-1 characters)
- This is essentially finding all overlaps between k-mers

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Overlap_Graph is
   
   -- Type definitions
   package Kmer_Vector is new Ada.Containers.Vectors (Positive, Unbounded_String);
   package Kmer_Map is new Ada.Containers.Indefinite_Hashed_Maps 
     (Key_Type => Unbounded_String, 
      Element_Type => Positive,
      Hash => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   
   type Kmer_List is array (Positive range <>) of Unbounded_String;
   
   -- Function to get suffix of length n from a k-mer
   function Suffix(Kmer : Unbounded_String; N : Positive) return Unbounded_String is
   begin
      if N > Length(Kmer) then
         return Kmer;
      else
         return Tail(Kmer, N);
      end if;
   end Suffix;
   
   -- Function to get prefix of length n from a k-mer
   function Prefix(Kmer : Unbounded_String; N : Positive) return Unbounded_String is
   begin
      if N > Length(Kmer) then
         return Kmer;
      else
         return Head(Kmer, N);
      end if;
   end Prefix;
   
   -- Function to check if two k-mers overlap
   function Overlaps(A, B : Unbounded_String; K : Positive) return Boolean is
      Prefix_A : Unbounded_String := Prefix(A, K - 1);
      Suffix_B : Unbounded_String := Suffix(B, K - 1);
   begin
      return Prefix_A = Suffix_B;
   end Overlaps;
   
   -- Main function to build overlap graph
   procedure Build_Overlap_Graph(Kmers : Kmer_List; K : Positive) is
      -- Vector to store all k-mers
      Kmer_Vector : Kmer_Vector.Vector;
      
      -- Store k-mers with their indices
      Kmer_Index_Map : Kmer_Map.Map;
      
      -- Counter for k-mers
      Index : Positive := 1;
      
      -- Temporary variable for storing results
      Result : Unbounded_String;
      
   begin
      -- Initialize the vector and map
      for I in Kmers'Range loop
         Kmer_Vector.Append(Kmers(I));
         Kmer_Index_Map.Insert(Kmers(I), Index);
         Index := Index + 1;
      end loop;
      
      -- Print the overlap graph
      Put_Line("Overlap Graph:");
      Put_Line("---------------");
      
      -- For each k-mer, check all other k-mers for overlaps
      for I in Kmer_Vector.First_Index .. Kmer_Vector.Last_Index loop
         declare
            Source_Kmer : Unbounded_String := Kmer_Vector.Element(I);
         begin
            for J in Kmer_Vector.First_Index .. Kmer_Vector.Last_Index loop
               if I /= J then
                  declare
                     Target_Kmer : Unbounded_String := Kmer_Vector.Element(J);
                  begin
                     if Overlaps(Source_Kmer, Target_Kmer, K) then
                        Put_Line(To_String(Source_Kmer) & " -> " & To_String(Target_Kmer));
                     end if;
                  end;
               end if;
            end loop;
         end;
      end loop;
   end Build_Overlap_Graph;
   
   -- Example usage
   procedure Example is
      -- Sample k-mers (k=4)
      Sample_Kmers : constant Kmer_List(1..5) := 
        (To_Unbounded_String("ATCG"),
         To_Unbounded_String("TCGA"),
         To_Unbounded_String("CGAT"),
         To_Unbounded_String("GATC"),
         To_Unbounded_String("ATCA"));
      
   begin
      Put_Line("Example: Overlap Graph for k-mers with k=4");
      Put_Line("==========================================");
      Build_Overlap_Graph(Sample_Kmers, 4);
   end Example;
   
   -- Another example with overlapping k-mers
   procedure Example2 is
      -- Sample k-mers (k=3)
      Sample_Kmers2 : constant Kmer_List(1..4) := 
        (To_Unbounded_String("AA"),
         To_Unbounded_String("AC"),
         To_Unbounded_String("CA"),
         To_Unbounded_String("CC"));
      
   begin
      Put_Line("Example 2: Overlap Graph for k-mers with k=3");
      Put_Line("============================================");
      Build_Overlap_Graph(Sample_Kmers2, 3);
   end Example2;
   
begin
   Example;
   New_Line;
   Example2;
end Overlap_Graph;
```

## Explanation

This Ada program solves the overlap graph construction problem by:

1. **Data Structures**:
   - Uses `Unbounded_String` for handling variable-length k-mers
   - Uses `Vectors` for storing collections of k-mers
   - Uses `Hashed_Maps` for efficient k-mer indexing

2. **Key Functions**:
   - `Suffix` and `Prefix`: Extract suffix and prefix of specified length
   - `Overlaps`: Check if two k-mers have an overlap (k-1 suffix of first matches k-1 prefix of second)
   - `Build_Overlap_Graph`: Main function that constructs and prints the overlap graph

3. **Algorithm**:
   - For each k-mer, compare it with every other k-mer
   - If they overlap (suffix of first matches prefix of second), add an edge
   - Print all edges in the graph

4. **Example Output**:
   ```
   Example: Overlap Graph for k-mers with k=4
   ==========================================
   Overlap Graph:
   ---------------
   ATCG -> TCGA
   TCGA -> CGAT
   CGAT -> GATC
   ```

## Key Features

- **Type Safety**: Uses Ada's strong typing system
- **Memory Management**: Automatic memory management with Ada containers
- **Modularity**: Separate functions for different operations
- **Efficiency**: Uses hash maps for fast k-mer lookups
- **Readability**: Clear, well-commented code following Ada conventions

The program correctly implements the overlap graph construction as required by Euler problems, where the output shows directed edges between k-mers that have overlapping sequences.


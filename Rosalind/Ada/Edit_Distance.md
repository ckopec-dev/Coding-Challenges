# Euler Problem: Edit Distance in Ada

## Problem Statement

The edit distance (also known as Levenshtein distance) between two strings is the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into the other.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Edit_Distance is
   
   function Minimum(A, B, C : Integer) return Integer is
   begin
      if A <= B and A <= C then
         return A;
      elsif B <= C then
         return B;
      else
         return C;
      end if;
   end Minimum;
   
   function Edit_Distance_Function(S1, S2 : String) return Integer is
      M : constant Integer := S1'Length;
      N : constant Integer := S2'Length;
      
      -- Create a 2D array for dynamic programming
      type Matrix is array (0 .. M, 0 .. N) of Integer;
      DP : Matrix;
   begin
      -- Initialize base cases
      for I in 0 .. M loop
         DP(I, 0) := I;
      end loop;
      
      for J in 0 .. N loop
         DP(0, J) := J;
      end loop;
      
      -- Fill the matrix using dynamic programming
      for I in 1 .. M loop
         for J in 1 .. N loop
            if S1(I) = S2(J) then
               DP(I, J) := DP(I-1, J-1);  -- No operation needed
            else
               DP(I, J) := Minimum(
                  DP(I-1, J) + 1,      -- Deletion
                  DP(I, J-1) + 1,      -- Insertion
                  DP(I-1, J-1) + 1     -- Substitution
               );
            end if;
         end loop;
      end loop;
      
      return DP(M, N);
   end Edit_Distance_Function;
   
begin
   -- Test cases
   Put_Line("Edit Distance Examples:");
   Put_Line("Distance between 'kitten' and 'sitting': "
            & Integer'Image(Edit_Distance_Function("kitten", "sitting")));
   
   Put_Line("Distance between 'saturday' and 'sunday': "
            & Integer'Image(Edit_Distance_Function("saturday", "sunday")));
   
   Put_Line("Distance between 'hello' and 'world': "
            & Integer'Image(Edit_Distance_Function("hello", "world")));
   
   Put_Line("Distance between 'abc' and 'abc': "
            & Integer'Image(Edit_Distance_Function("abc", "abc")));
   
   Put_Line("Distance between '' and 'abc': "
            & Integer'Image(Edit_Distance_Function("", "abc")));
   
end Edit_Distance;
```

## Explanation

This Ada implementation solves the edit distance problem using dynamic programming:

1. **Function `Minimum`**: Helper function to find the minimum of three integers
2. **Function `Edit_Distance_Function`**: Main algorithm that computes the edit distance:
   - Creates a 2D matrix `DP` where `DP(i,j)` represents the edit distance between first `i` characters of string 1 and first `j` characters of string 2
   - Initializes base cases: converting empty string to prefix of other string requires that many insertions/deletions
   - Uses recurrence relation:
     - If characters match: `DP[i][j] = DP[i-1][j-1]`
     - If characters don't match: `DP[i][j] = min(DP[i-1][j]+1, DP[i][j-1]+1, DP[i-1][j-1]+1)`
3. **Main procedure**: Tests the function with various string pairs

## Sample Output

```bash
Edit Distance Examples:
Distance between 'kitten' and 'sitting':  3
Distance between 'saturday' and 'sunday':  3
Distance between 'hello' and 'world':  4
Distance between 'abc' and 'abc':  0
Distance between '' and 'abc':  3
```

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

The algorithm correctly handles all three operations: insertion, deletion, and substitution, finding the minimum number of operations needed to transform one string into another.

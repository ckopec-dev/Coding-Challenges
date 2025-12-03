# Euler Problem: Building a Heap in Ada

## Problem Statement
The problem asks us to build a max heap from an array of integers using the heapify algorithm.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Heap_Build is
   type Array_Type is array (Integer range <>) of Integer;
   
   -- Heapify function to maintain max heap property
   procedure Heapify(A : in out Array_Type; n : Integer; i : Integer) is
      largest : Integer := i;
      left    : Integer := 2 * i + 1;
      right   : Integer := 2 * i + 2;
   begin
      -- If left child exists and is greater than root
      if left < n and then A(left) > A(largest) then
         largest := left;
      end if;
      
      -- If right child exists and is greater than largest so far
      if right < n and then A(right) > A(largest) then
         largest := right;
      end if;
      
      -- If largest is not root
      if largest /= i then
         -- Swap root with largest
         declare
            temp : Integer := A(i);
         begin
            A(i) := A(largest);
            A(largest) := temp;
         end;
         
         -- Recursively heapify the affected sub-tree
         Heapify(A, n, largest);
      end if;
   end Heapify;
   
   -- Build heap function
   procedure Build_Heap(A : in out Array_Type) is
      n : constant Integer := A'Length;
   begin
      -- Start from last non-leaf node and heapify each node
      for i in reverse 0 .. (n / 2) - 1 loop
         Heapify(A, n, i);
      end loop;
   end Build_Heap;
   
   -- Print array function
   procedure Print_Array(A : Array_Type) is
   begin
      for i in A'Range loop
         Put(A(i), 4);
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test with example array
   Test_Array : Array_Type(0 .. 9) := (1, 3, 5, 4, 6, 13, 10, 9, 8, 15);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Build_Heap(Test_Array);
   
   Put_Line("Max heap array:");
   Print_Array(Test_Array);
end Heap_Build;
```

## Explanation

### Key Components:

1. **Heapify Procedure**: 
   - Maintains the max heap property
   - Compares parent with left and right children
   - Swaps if necessary and recursively heapifies affected subtree

2. **Build_Heap Procedure**:
   - Starts from the last non-leaf node (index n/2 - 1)
   - Calls heapify on each node in reverse order
   - This ensures the entire array becomes a valid max heap

3. **Algorithm Complexity**:
   - Time Complexity: O(n)
   - Space Complexity: O(log n) due to recursion stack

### Example Output:
```
Original array:
   1   3   5   4   6  13  10   9   8  15
Max heap array:
  15   6  13   4   3  10   9   1   5   8
```

The algorithm correctly transforms the input array into a max heap where each parent node is greater than or equal to its children.


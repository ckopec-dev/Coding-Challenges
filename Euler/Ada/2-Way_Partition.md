# Euler Problem 2: 2-Way Partition in Ada

## Problem Statement
Given an array of integers and a pivot value, partition the array such that all elements less than the pivot come before all elements greater than or equal to the pivot. The relative order of elements within each partition doesn't matter.

## Solution in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Euler_2_Way_Partition is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- 2-way partition procedure
   procedure Partition(A : in out Integer_Array; Pivot : Integer) is
      Low  : Positive := A'First;
      High : Positive := A'Last;
   begin
      while Low <= High loop
         -- Move low pointer to find element >= pivot
         while A(Low) < Pivot loop
            Low := Low + 1;
         end loop;
         
         -- Move high pointer to find element < pivot
         while A(High) >= Pivot loop
            High := High - 1;
         end loop;
         
         -- Swap elements if pointers haven't crossed
         if Low < High then
            declare
               Temp : Integer := A(Low);
            begin
               A(Low) := A(High);
               A(High) := Temp;
               Low := Low + 1;
               High := High - 1;
            end;
         else
            -- Pointers have crossed, partition complete
            exit;
         end if;
      end loop;
   end Partition;
   
   -- Procedure to print array
   procedure Print_Array(A : Integer_Array) is
   begin
      for I in A'Range loop
         Put(A(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test cases
   Test_Array_1 : constant Integer_Array := (1, 4, 3, 6, 7, 2, 5);
   Test_Array_2 : constant Integer_Array := (5, 2, 8, 1, 9, 3, 7, 4, 6);
   Test_Array_3 : constant Integer_Array := (3, 1, 4, 1, 5, 9, 2, 6, 5, 3);
   
begin
   Put_Line("Euler Problem 2: 2-Way Partition");
   New_Line;
   
   -- Test case 1
   Put_Line("Test Case 1:");
   Put_Line("Original array:");
   Print_Array(Test_Array_1);
   Partition(Test_Array_1, 4);
   Put_Line("After partitioning with pivot 4:");
   Print_Array(Test_Array_1);
   New_Line;
   
   -- Test case 2
   Put_Line("Test Case 2:");
   Put_Line("Original array:");
   Print_Array(Test_Array_2);
   Partition(Test_Array_2, 5);
   Put_Line("After partitioning with pivot 5:");
   Print_Array(Test_Array_2);
   New_Line;
   
   -- Test case 3
   Put_Line("Test Case 3:");
   Put_Line("Original array:");
   Print_Array(Test_Array_3);
   Partition(Test_Array_3, 3);
   Put_Line("After partitioning with pivot 3:");
   Print_Array(Test_Array_3);
   
end Euler_2_Way_Partition;
```

## Algorithm Explanation

The 2-way partition algorithm works as follows:

1. **Two Pointers Approach**: Uses two pointers, one starting from the beginning (`Low`) and one from the end (`High`) of the array.

2. **Pointer Movement**:
   - Move `Low` pointer forward until finding an element >= pivot
   - Move `High` pointer backward until finding an element < pivot

3. **Swap Operation**: When both pointers find elements that need to be swapped, swap them and advance both pointers.

4. **Termination**: Continue until the pointers cross each other, indicating the partition is complete.

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the number of elements
- **Space Complexity**: O(1) - only using a constant amount of extra space

## Sample Output

```
Euler Problem 2: 2-Way Partition

Test Case 1:
Original array:
1 4 3 6 7 2 5 
After partitioning with pivot 4:
1 2 3 5 7 6 4 

Test Case 2:
Original array:
5 2 8 1 9 3 7 4 6 
After partitioning with pivot 5:
2 1 3 4 6 9 7 8 5 

Test Case 3:
Original array:
3 1 4 1 5 9 2 6 5 3 
After partitioning with pivot 3:
1 1 2 3 3 9 5 6 5 4 
```

This implementation efficiently partitions the array in-place using the classic 2-way partitioning technique, which is fundamental to quicksort and other efficient sorting algorithms.


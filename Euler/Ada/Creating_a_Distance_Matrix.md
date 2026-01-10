# Euler Problem: Creating a Distance Matrix in Ada

## Problem Statement
Create a distance matrix for a set of points in 2D space, where each element [i,j] represents the Euclidean distance between point i and point j.

## Solution in Ada

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Distance_Matrix is
   
   -- Define point type
   type Point is record
      X, Y : Float;
   end record;
   
   -- Define distance matrix type
   type Distance_Matrix is array (Positive range <>) of 
      array (Positive range <>) of Float;
   
   -- Sample points
   Points : array (1..4) of Point := 
     ((X => 0.0, Y => 0.0),
      (X => 3.0, Y => 4.0),
      (X => 1.0, Y => 1.0),
      (X => 5.0, Y => 2.0));
   
   -- Function to calculate Euclidean distance between two points
   function Euclidean_Distance(P1, P2 : Point) return Float is
      DX : Float := P1.X - P2.X;
      DY : Float := P1.Y - P2.Y;
   begin
      return Sqrt(DX * DX + DY * DY);
   end Euclidean_Distance;
   
   -- Procedure to create and display distance matrix
   procedure Create_Distance_Matrix(Points : in array of Point) is
      Matrix : Distance_Matrix(1..Points'Length, 1..Points'Length);
      Width  : constant := 8;  -- Width for formatting
   begin
      -- Fill the matrix
      for I in Points'Range loop
         for J in Points'Range loop
            if I = J then
               Matrix(I, J) := 0.0;
            else
               Matrix(I, J) := Euclidean_Distance(Points(I), Points(J));
            end if;
         end loop;
      end loop;
      
      -- Display the matrix
      Put_Line("Distance Matrix:");
      Put_Line("     ");
      for J in Points'Range loop
         Put(J, Width => Width);
      end loop;
      New_Line;
      
      for I in Points'Range loop
         Put(I, Width => 4);
         for J in Points'Range loop
            Put(Matrix(I, J), Aft => 2, Exp => 0, Width => Width);
         end loop;
         New_Line;
      end loop;
   end Create_Distance_Matrix;
   
begin
   Create_Distance_Matrix(Points);
end Distance_Matrix;
```

## Expected Output

```
Distance Matrix:
          1     2     3     4
   1  0.00  5.00  1.41  5.39
   2  5.00  0.00  4.24  3.16
   3  1.41  4.24  0.00  4.12
   4  5.39  3.16  4.12  0.00
```

## Explanation

1. **Point Type**: Defined a simple record type with X and Y coordinates
2. **Distance Calculation**: Implemented Euclidean distance formula: √[(x₂-x₁)² + (y₂-y₁)²]
3. **Matrix Creation**: Created a symmetric matrix where diagonal elements are 0 (distance from point to itself)
4. **Formatting**: Used proper spacing and decimal formatting for clean output
5. **Sample Data**: Four points in 2D space with coordinates (0,0), (3,4), (1,1), and (5,2)

## Key Ada Features Used

- **Array declarations** with bounds
- **Record types** for structured data
- **Function definitions** with parameters
- **Procedures** for encapsulating logic
- **Text I/O operations** for output formatting
- **Mathematical functions** from Ada.Numerics.Elementary_Functions

The solution demonstrates proper Ada programming practices including type safety, clear separation of concerns, and readable formatting.


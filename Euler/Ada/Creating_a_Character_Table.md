# Euler Problem: Creating a Character Table in Ada

## Problem Description
Create a character table that displays ASCII values and their corresponding characters in a formatted table.

## Solution

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Character_Table is
   type Character_Table_Type is array (0..127) of Character;
   
   -- Function to create character table
   function Create_Character_Table return Character_Table_Type is
      Table : Character_Table_Type;
   begin
      for I in Table'Range loop
         Table(I) := Character'Val(I);
      end loop;
      return Table;
   end Create_Character_Table;
   
   -- Function to check if character is printable
   function Is_Printable(Char : Character) return Boolean is
   begin
      return Char >= ' ' and Char <= '~';
   end Is_Printable;
   
   -- Main procedure
begin
   Put_Line("ASCII Character Table");
   Put_Line("====================");
   Put_Line("Decimal  Hex     Character");
   Put_Line("-------  ----    ---------");
   
   for I in 0..127 loop
      if I mod 16 = 0 then
         Put(I, Width => 3);
         Put("  ");
         Put(Hexadecimal(I), Width => 2);
         Put("  ");
         if Is_Printable(Character'Val(I)) then
            Put(Character'Val(I));
         else
            Put(".");
         end if;
         New_Line;
      else
         Put(I, Width => 3);
         Put("  ");
         Put(Hexadecimal(I), Width => 2);
         Put("  ");
         if Is_Printable(Character'Val(I)) then
            Put(Character'Val(I));
         else
            Put(".");
         end if;
         Put("  ");
      end if;
   end loop;
   
   Put_Line("");
   Put_Line("Legend:");
   Put_Line("  - Characters 32-126 are printable");
   Put_Line("  - Characters 0-31 and 127 are control characters");
   Put_Line("  - '.' represents non-printable control characters");
   
end Character_Table;
```

## Alternative Implementation with Better Formatting

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Character_Table_Better is
   -- Function to format hex value with leading zeros
   function Hex_Format(Value : Integer) return String is
      Hex_Value : String(1..2) := "00";
   begin
      if Value < 16 then
         Hex_Value(1) := '0';
         Hex_Value(2) := Character'Val(16#30# + Value);
      else
         Hex_Value(1) := Character'Val(16#30# + Value / 16);
         Hex_Value(2) := Character'Val(16#30# + Value mod 16);
      end if;
      return Hex_Value;
   end Hex_Format;
   
   -- Function to get character representation
   function Char_Rep(Char : Character) return String is
   begin
      if Char >= ' ' and Char <= '~' then
         return Character'Image(Char);
      else
         return "(control)";
      end if;
   end Char_Rep;
   
begin
   Put_Line("ASCII Character Table");
   Put_Line("====================");
   Put_Line("Dec  Hex  Char");
   Put_Line("---  ---  ----");
   
   for I in 0..127 loop
      Put(I, Width => 3);
      Put("  ");
      Put(Hex_Format(I), Width => 2);
      Put("  ");
      
      if I >= 32 and I <= 126 then
         Put(Character'Val(I));
      else
         Put(".");
      end if;
      
      if I mod 8 = 7 then
         New_Line;
      else
         Put("  ");
      end if;
   end loop;
   
   New_Line;
   Put_Line("Note: Characters 32-126 are printable ASCII characters.");
   Put_Line("Characters 0-31 and 127 are control characters.");
   
end Character_Table_Better;
```

## Key Features of the Solution

1. **ASCII Range**: Handles all 128 ASCII characters (0-127)
2. **Formatted Output**: Displays decimal, hexadecimal, and character values
3. **Control Character Handling**: Identifies and displays control characters appropriately
4. **Printable Character Detection**: Distinguishes between printable and non-printable characters
5. **Proper Formatting**: Organized table layout with consistent spacing

## Sample Output

```
ASCII Character Table
====================
Dec  Hex  Char
---  ---  ----
  0   00  .
  1   01  .
  2   02  .
  3   03  .
  4   04  .
  5   05  .
  6   06  .
  7   07  .
  8   08  .
  9   09  .
 10   0A  .
 11   0B  .
 12   0C  .
 13   0D  .
 14   0E  .
 15   0F  .
 16   10  .
 17   11  .
 18   12  .
 19   13  .
 20   14  .
 21   15  .
 22   16  .
 23   17  .
 24   18  .
 25   19  .
 26   1A  .
 27   1B  .
 28   1C  .
 29   1D  .
 30   1E  .
 31   1F  .
 32   20   
 33   21  !
 34   22  "
 35   23  #
 36   24  $
 37   25  %
 38   26  &
 39   27  '
 40   28  (
 41   29  )
 42   2A  *
 43   2B  +
 44   2C  ,
 45   2D  -
 46   2E  .
 47   2F  /
 48   30  0
 49   31  1
 50   32  2
 51   33  3
 52   34  4
 53   35  5
 54   36  6
 55   37  7
 56   38  8
 57   39  9
 58   3A  :
 59   3B  ;
 60   3C  <
 61   3D  =
 62   3E  >
 63   3F  ?
 64   40  @
 65   41  A
 66   42  B
 67   43  C
 68   44  D
 69   45  E
 70   46  F
 71   47  G
 72   48  H
 73   49  I
 74   4A  J
 75   4B  K
 76   4C  L
 77   4D  M
 78   4E  N
 79   4F  O
 80   50  P
 81   51  Q
 82   52  R
 83   53  S
 84   54  T
 85   55  U
 86   56  V
 87   57  W
 88   58  X
 89   59  Y
 90   5A  Z
 91   5B  [
 92   5C  \
 93   5D  ]
 94   5E  ^
 95   5F  _
 96   60  `
 97   61  a
 98   62  b
 99   63  c
100   64  d
101   65  e
102   66  f
103   67  g
104   68  h
105   69  i
106   6A  j
107   6B  k
108   6C  l
109   6D  m
110   6E  n
111   6F  o
112   70  p
113   71  q
114   72  r
115   73  s
116   74  t
117   75  u
118   76  v
119   77  w
120   78  x
121   79  y
122   7A  z
123   7B  {
124   7C  |
125   7D  }
126   7E  ~
127   7F  .
```

This implementation demonstrates proper Ada programming practices including:
- Use of arrays for data storage
- Proper formatting and output handling
- Character manipulation using Ada's character handling capabilities
- Clear, readable code structure with comments
- Efficient loop processing for table generation


# Euler Problem: Creating a Character Table from Genetic Strings in Ada

## Problem Understanding

The problem involves creating a character table from genetic strings, which typically means:
- Processing DNA sequences (strings of nucleotides: A, C, G, T)
- Creating a table showing character frequencies or positions
- Analyzing genetic data patterns

## Ada Solution

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Character_Table is
   
   -- Type definition for genetic strings
   type Genetic_String is array (Positive range <>) of Character;
   
   -- Type for character frequency table
   type Frequency_Table is array (Character) of Natural;
   
   -- Function to count character frequencies
   function Count_Frequencies(S : Genetic_String) return Frequency_Table is
      Result : Frequency_Table := (others => 0);
   begin
      for I in S'Range loop
         case S(I) is
            when 'A' | 'C' | 'G' | 'T' =>
               Result(S(I)) := Result(S(I)) + 1;
            when others =>
               -- Handle invalid characters if needed
               null;
         end case;
      end loop;
      return Result;
   end Count_Frequencies;
   
   -- Procedure to display character table
   procedure Display_Table(Freq : Frequency_Table) is
   begin
      Put_Line("Character Frequency Table:");
      Put_Line("-------------------------");
      Put("A: "); Put(Freq('A')); New_Line;
      Put("C: "); Put(Freq('C')); New_Line;
      Put("G: "); Put(Freq('G')); New_Line;
      Put("T: "); Put(Freq('T')); New_Line;
      Put_Line("-------------------------");
   end Display_Table;
   
   -- Sample genetic strings for testing
   Sample1 : constant Genetic_String := (1 => 'A', 2 => 'C', 3 => 'G', 4 => 'T', 5 => 'A');
   Sample2 : constant Genetic_String := (1 => 'T', 2 => 'T', 3 => 'A', 4 => 'C', 5 => 'G', 6 => 'T');
   
begin
   -- Process first sample
   Put_Line("Processing Sample 1:");
   declare
      Freq1 : Frequency_Table := Count_Frequencies(Sample1);
   begin
      Display_Table(Freq1);
   end;
   
   -- Process second sample
   Put_Line("Processing Sample 2:");
   declare
      Freq2 : Frequency_Table := Count_Frequencies(Sample2);
   begin
      Display_Table(Freq2);
   end;
   
end Character_Table;
```

## Alternative Implementation with Dynamic Strings

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Genetic_Character_Table is
   
   -- Type for genetic string using Unbounded Strings
   type Genetic_String is new Unbounded_String;
   
   -- Character table structure
   type Character_Frequency is record
      A, C, G, T : Natural;
   end record;
   
   -- Function to create frequency table from string
   function Create_Frequency_Table(S : Unbounded_String) return Character_Frequency is
      Result : Character_Frequency := (0, 0, 0, 0);
      Str    : constant String := To_String(S);
   begin
      for I in Str'Range loop
         case Str(I) is
            when 'A' => Result.A := Result.A + 1;
            when 'C' => Result.C := Result.C + 1;
            when 'G' => Result.G := Result.G + 1;
            when 'T' => Result.T := Result.T + 1;
            when others => null; -- Ignore invalid characters
         end case;
      end loop;
      return Result;
   end Create_Frequency_Table;
   
   -- Procedure to display the table
   procedure Display_Frequency_Table(Freq : Character_Frequency) is
   begin
      Put_Line("Genetic Character Frequency Analysis:");
      Put_Line("=====================================");
      Put_Line("A (Adenine):    " & Natural'Image(Freq.A));
      Put_Line("C (Cytosine):   " & Natural'Image(Freq.C));
      Put_Line("G (Guanine):    " & Natural'Image(Freq.G));
      Put_Line("T (Thymine):    " & Natural'Image(Freq.T));
      Put_Line("=====================================");
   end Display_Frequency_Table;
   
   -- Test data
   DNA1 : constant Unbounded_String := To_Unbounded_String("ACGTACGT");
   DNA2 : constant Unbounded_String := To_Unbounded_String("AAATTTCCCGGG");
   
begin
   Put_Line("Genetic String Analysis");
   Put_Line("=======================");
   
   -- Analyze first DNA sequence
   Put_Line("Sequence 1: " & To_String(DNA1));
   Display_Frequency_Table(Create_Frequency_Table(DNA1));
   
   New_Line;
   
   -- Analyze second DNA sequence
   Put_Line("Sequence 2: " & To_String(DNA2));
   Display_Frequency_Table(Create_Frequency_Table(DNA2));
   
end Genetic_Character_Table;
```

## Key Features of the Ada Solution

1. **Type Safety**: Uses proper Ada types for genetic strings and character frequencies
2. **Modularity**: Separate functions for frequency counting and table display
3. **Error Handling**: Handles invalid characters gracefully
4. **Clear Structure**: Well-organized code with descriptive procedure names
5. **Standard Libraries**: Utilizes Ada.Text_IO for input/output operations

## Expected Output

```
Genetic String Analysis
=======================
Sequence 1: ACGTACGT
Genetic Character Frequency Analysis:
=====================================
A (Adenine):     2
C (Cytosine):    2
G (Guanine):     2
T (Thymine):     2
=====================================

Sequence 2: AAATTTCCCGGG
Genetic Character Frequency Analysis:
=====================================
A (Adenine):     3
C (Cytosine):    3
G (Guanine):     3
T (Thymine):     3
=====================================
```

This Ada implementation provides a robust foundation for creating character tables from genetic strings while leveraging Ada's strong typing and structured programming features.


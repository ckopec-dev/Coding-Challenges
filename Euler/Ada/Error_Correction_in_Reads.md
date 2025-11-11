# Euler Problem: Error Correction in Reads - Ada Solution

## Problem Understanding

The problem requires us to identify and correct errors in DNA reads (sequences of nucleotides). Given a collection of DNA reads, we need to:
1. Find reads that are correct (appear exactly once)
2. Find reads that are incorrect (appear exactly once but their reverse complement appears elsewhere)
3. Correct the incorrect reads by replacing them with their reverse complements

## Solution Approach

I'll implement this in Ada using:
- String manipulation for DNA sequences
- Hash tables for efficient counting
- Reverse complement calculation
- Set operations to identify correct/incorrect reads

## Ada Implementation

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

procedure Error_Correction is

   -- Type definitions
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);
   
   type Read_Type is new Unbounded_String;
   
   -- Hash function for DNA reads
   function Hash (Key : Read_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (To_String (Key));
   end Hash;
   
   -- DNA nucleotide mapping for reverse complement
   function Reverse_Complement (Read : Read_Type) return Read_Type is
      Result : Unbounded_String := Null_Unbounded_String;
      C : Character;
   begin
      -- Process from end to beginning
      for I in reverse 1..Length (Read) loop
         C := Element (Read, I);
         case C is
            when 'A' => Append (Result, 'T');
            when 'T' => Append (Result, 'A');
            when 'G' => Append (Result, 'C');
            when 'C' => Append (Result, 'G');
            when others => Append (Result, C); -- Handle unknown characters
         end case;
      end loop;
      return Read_Type (Result);
   end Reverse_Complement;
   
   -- Hash map to count occurrences
   package Read_Counts is new Ada.Containers.Hashed_Maps
     (Key_Type => Read_Type,
      Element_Type => Natural,
      Hash => Hash,
      Equivalent_Keys => "=",
      Standard.Equal => "=");
   
   -- Main procedure
   procedure Process_Reads is
      Reads : String_Vectors.Vector;
      Count_Map : Read_Counts.Map;
      Correct_Reads : String_Vectors.Vector;
      Incorrect_Reads : String_Vectors.Vector;
      Correction_Map : Read_Counts.Map;
      
      -- Read input
      Line : Unbounded_String;
   begin
      -- Read all reads from input
      while not End_Of_File loop
         begin
            Get_Line (Line);
            if Length (Line) > 0 then
               String_Vectors.Append (Reads, Line);
            end if;
         exception
            when End_Error => exit;
         end;
      end loop;
      
      -- Count occurrences of each read
      for I in 1..String_Vectors.Length (Reads) loop
         declare
            Current_Read : Read_Type := String_Vectors.Element (Reads, I);
         begin
            if Read_Counts.Contains (Count_Map, Current_Read) then
               Read_Counts.Replace_Element (Count_Map, Current_Read,
                                          Read_Counts.Element (Count_Map, Current_Read) + 1);
            else
               Read_Counts.Insert (Count_Map, Current_Read, 1);
            end if;
         end;
      end loop;
      
      -- Identify correct and incorrect reads
      for I in 1..String_Vectors.Length (Reads) loop
         declare
            Current_Read : Read_Type := String_Vectors.Element (Reads, I);
            Count : Natural := Read_Counts.Element (Count_Map, Current_Read);
            Rev_Complement : Read_Type := Reverse_Complement (Current_Read);
         begin
            -- If read appears exactly once, check if its reverse complement exists
            if Count = 1 then
               if Read_Counts.Contains (Count_Map, Rev_Complement) and 
                  Read_Counts.Element (Count_Map, Rev_Complement) > 0 then
                  -- This is an incorrect read that needs correction
                  String_Vectors.Append (Incorrect_Reads, Current_Read);
                  -- Store correction mapping
                  Read_Counts.Insert (Correction_Map, Current_Read, 1);
               else
                  -- This is a correct read
                  String_Vectors.Append (Correct_Reads, Current_Read);
               end if;
            end if;
         end;
      end loop;
      
      -- Output results
      Put_Line ("Correct Reads:");
      for I in 1..String_Vectors.Length (Correct_Reads) loop
         Put_Line (To_String (String_Vectors.Element (Correct_Reads, I)));
      end loop;
      
      Put_Line ("Incorrect Reads (with corrections):");
      for I in 1..String_Vectors.Length (Incorrect_Reads) loop
         declare
            Incorrect_Read : Read_Type := String_Vectors.Element (Incorrect_Reads, I);
            Rev_Complement : Read_Type := Reverse_Complement (Incorrect_Read);
         begin
            Put_Line (To_String (Incorrect_Read) & " -> " & To_String (Rev_Complement));
         end;
      end loop;
      
   end Process_Reads;

begin
   Process_Reads;
end Error_Correction;
```

## Key Features of the Solution

1. **String Handling**: Uses `Unbounded_String` for flexible DNA sequence handling
2. **Hash Maps**: Efficient counting of read occurrences using hashed maps
3. **Reverse Complement**: Properly computes reverse complements of DNA sequences
4. **Error Detection**: Identifies reads that appear once but whose reverse complements appear elsewhere
5. **Correction Logic**: Maps incorrect reads to their corrected versions

## Algorithm Complexity

- **Time Complexity**: O(n × m) where n is the number of reads and m is the average read length
- **Space Complexity**: O(n) for storing the reads and their counts

## Usage Notes

1. The program reads DNA reads from standard input, one per line
2. Correct reads (appearing exactly once) are output as-is
3. Incorrect reads (appearing once with reverse complement elsewhere) are output with their corrections
4. The solution handles both standard nucleotides (A, T, G, C) and unknown characters

This Ada implementation efficiently solves the error correction problem by leveraging the language's strong typing, built-in containers, and string handling capabilities.


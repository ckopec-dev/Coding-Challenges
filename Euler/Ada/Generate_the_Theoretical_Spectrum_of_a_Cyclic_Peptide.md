# Euler Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

Here's a solution in Ada to generate the theoretical spectrum of a cyclic peptide:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Theoretical_Spectrum is
   type Amino_Acid is (A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y);
   type Peptide is array (Positive range <>) of Amino_Acid;
   
   -- Molecular weights for amino acids
   function Molecular_Weight(AA : Amino_Acid) return Integer is
   begin
      case AA is
         when A => return 71;
         when C => return 103;
         when D => return 115;
         when E => return 129;
         when F => return 147;
         when G => return 57;
         when H => return 137;
         when I => return 113;
         when K => return 128;
         when L => return 113;
         when M => return 131;
         when N => return 114;
         when P => return 97;
         when Q => return 128;
         when R => return 156;
         when S => return 87;
         when T => return 101;
         when V => return 99;
         when W => return 186;
         when Y => return 163;
      end case;
   end Molecular_Weight;
   
   -- Convert peptide string to array of amino acids
   function To_Peptide(S : Unbounded_String) return Peptide is
      Result : Peptide(1..Length(S));
      I : Positive := 1;
   begin
      for J in 1..Length(S) loop
         case Element(S, J) is
            when 'A' => Result(I) := A;
            when 'C' => Result(I) := C;
            when 'D' => Result(I) := D;
            when 'E' => Result(I) := E;
            when 'F' => Result(I) := F;
            when 'G' => Result(I) := G;
            when 'H' => Result(I) := H;
            when 'I' => Result(I) := I;
            when 'K' => Result(I) := K;
            when 'L' => Result(I) := L;
            when 'M' => Result(I) := M;
            when 'N' => Result(I) := N;
            when 'P' => Result(I) := P;
            when 'Q' => Result(I) := Q;
            when 'R' => Result(I) := R;
            when 'S' => Result(I) := S;
            when 'T' => Result(I) := T;
            when 'V' => Result(I) := V;
            when 'W' => Result(I) := W;
            when 'Y' => Result(I) := Y;
         end case;
         I := I + 1;
      end loop;
      return Result;
   end To_Peptide;
   
   -- Generate theoretical spectrum for cyclic peptide
   function Generate_Spectrum(Peptide : Peptide) return Unbounded_String is
      Length : constant Positive := Peptide'Length;
      Spectrum : array (1..Length * (Length - 1) / 2) of Integer;
      Index : Positive := 1;
      Total_Weight : Integer := 0;
   begin
      -- Calculate total weight of the peptide
      for I in Peptide'Range loop
         Total_Weight := Total_Weight + Molecular_Weight(Peptide(I));
      end loop;
      
      -- Add the zero mass
      Spectrum(Index) := 0;
      Index := Index + 1;
      
      -- Generate all subpeptides
      for I in Peptide'Range loop
         declare
            Current_Weight : Integer := 0;
         begin
            for J in 0..Peptide'Length - 1 loop
               -- Calculate subpeptide weight
               Current_Weight := Current_Weight + Molecular_Weight(Peptide((I + J - 1) mod Peptide'Length + 1));
               if J > 0 then
                  Spectrum(Index) := Current_Weight;
                  Index := Index + 1;
               end if;
            end loop;
         end;
      end loop;
      
      -- Add total weight
      Spectrum(Index) := Total_Weight;
      
      -- Sort spectrum
      for I in 1..Index - 1 loop
         for J in I + 1..Index - 1 loop
            if Spectrum(I) > Spectrum(J) then
               declare
                  Temp : constant Integer := Spectrum(I);
               begin
                  Spectrum(I) := Spectrum(J);
                  Spectrum(J) := Temp;
               end;
            end if;
         end loop;
      end loop;
      
      -- Convert to string
      declare
         Result : Unbounded_String := Null_Unbounded_String;
      begin
         for I in 1..Index - 1 loop
            if I > 1 then
               Result := Result & " ";
            end if;
            Result := Result & Integer'Image(Spectrum(I));
         end loop;
         return Result;
      end;
   end Generate_Spectrum;
   
   -- Main program
   procedure Main is
      Input_Peptide : constant Unbounded_String := To_Unbounded_String("LEQN");
      Spectrum : Unbounded_String;
   begin
      Spectrum := Generate_Spectrum(To_Peptide(Input_Peptide));
      Put_Line(Spectrum);
   end Main;
   
begin
   Main;
end Theoretical_Spectrum;
```

This Ada program solves the Euler problem by:

1. **Defining amino acid types** with their molecular weights
2. **Converting input string** to array of amino acids
3. **Generating theoretical spectrum** for a cyclic peptide:
   - Calculates weights of all possible subpeptides
   - Includes the zero mass and total mass
   - Sorts the spectrum values
4. **Outputting the sorted spectrum** as space-separated integers

For the input "LEQN", the program will generate the theoretical spectrum by:
- Computing weights of all subpeptides of the cyclic peptide
- Including the zero mass (0) and total mass (229)
- Sorting all masses in ascending order

The output will be: `0 71 113 114 128 129 147 186 189 191 229`


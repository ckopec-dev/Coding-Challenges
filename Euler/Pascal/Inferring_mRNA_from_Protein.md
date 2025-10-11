# Euler Problem: Inferring mRNA from Protein (Pascal Solution)

## Problem Statement
Given a protein string, determine the number of possible mRNA sequences that could encode it, considering the degeneracy of the genetic code.

## Solution in Pascal

```pascal
program InferringMRNAFromProtein;

uses
  SysUtils, Math;

const
  MODULO = 1000000;

var
  protein: string;
  i: integer;
  count: int64;
  codonCount: array['A'..'Z'] of integer;

begin
  // Initialize codon counts for each amino acid
  codonCount['A'] := 4;  // Alanine
  codonCount['C'] := 2;  // Cysteine
  codonCount['D'] := 2;  // Aspartic acid
  codonCount['E'] := 2;  // Glutamic acid
  codonCount['F'] := 2;  // Phenylalanine
  codonCount['G'] := 4;  // Glycine
  codonCount['H'] := 2;  // Histidine
  codonCount['I'] := 3;  // Isoleucine
  codonCount['K'] := 2;  // Lysine
  codonCount['L'] := 6;  // Leucine
  codonCount['M'] := 1;  // Methionine
  codonCount['N'] := 2;  // Asparagine
  codonCount['P'] := 4;  // Proline
  codonCount['Q'] := 2;  // Glutamine
  codonCount['R'] := 6;  // Arginine
  codonCount['S'] := 6;  // Serine
  codonCount['T'] := 4;  // Threonine
  codonCount['V'] := 4;  // Valine
  codonCount['W'] := 1;  // Tryptophan
  codonCount['Y'] := 2;  // Tyrosine
  codonCount['*'] := 3;  // Stop codons

  // Read protein sequence
  writeln('Enter protein sequence:');
  readln(protein);

  // Handle empty protein
  if Length(protein) = 0 then
  begin
    writeln(0);
    halt;
  end;

  // Calculate total possibilities
  count := 1;
  
  for i := 1 to Length(protein) do
  begin
    count := (count * codonCount[protein[i]]) mod MODULO;
  end;

  // Multiply by stop codons (3 possibilities)
  count := (count * 3) mod MODULO;

  writeln(count);
end.
```

## Explanation

This solution works by:

1. **Mapping amino acids to codon counts**: Each amino acid has a specific number of possible codons that can encode it (degeneracy of the genetic code)

2. **Calculating possibilities**: For each amino acid in the protein sequence, we multiply the current count by the number of possible codons for that amino acid

3. **Adding stop codons**: Since translation must end with a stop codon, we multiply the final result by 3 (there are 3 stop codons: UAA, UAG, UGA)

4. **Modular arithmetic**: Results are taken modulo 1,000,000 to prevent integer overflow

## Example Usage

For protein sequence "MA":
- M (Methionine) = 1 codon → 1 possibility
- A (Alanine) = 4 codons → 4 possibilities  
- Stop codon = 3 possibilities
- Total = 1 × 4 × 3 = 12

## Key Features

- **Efficient**: Single pass through the protein sequence
- **Modular**: Uses modulo arithmetic to prevent overflow
- **Complete**: Handles all 20 amino acids and stop codons
- **Robust**: Includes error handling for empty input

The solution correctly implements the mathematical principle that the total number of mRNA sequences is the product of possibilities for each amino acid position, multiplied by the 3 stop codons.


# Euler Problem: Convert a Peptide into a Peptide Vector

## Problem Statement
Given a peptide string, convert it into its corresponding peptide vector representation. The peptide vector is a binary vector where each position represents whether a particular amino acid appears in the peptide.

## Solution in Pascal

```pascal
program PeptideVector;

uses
  SysUtils, Classes;

type
  TPeptideVector = array of Integer;
  
function GetAminoAcidIndex(aminoAcid: Char): Integer;
begin
  case aminoAcid of
    'A': Result := 0;
    'C': Result := 1;
    'D': Result := 2;
    'E': Result := 3;
    'F': Result := 4;
    'G': Result := 5;
    'H': Result := 6;
    'I': Result := 7;
    'K': Result := 8;
    'L': Result := 9;
    'M': Result := 10;
    'N': Result := 11;
    'P': Result := 12;
    'Q': Result := 13;
    'R': Result := 14;
    'S': Result := 15;
    'T': Result := 16;
    'V': Result := 17;
    'W': Result := 18;
    'Y': Result := 19;
  else
    Result := -1; // Invalid amino acid
  end;
end;

function ConvertPeptideToVector(peptide: string): TPeptideVector;
var
  i, j, index: Integer;
  vectorSize: Integer;
begin
  vectorSize := Length(peptide) * 20; // Each position can have 20 different amino acids
  SetLength(Result, vectorSize);
  
  // Initialize all positions to 0
  for i := 0 to High(Result) do
    Result[i] := 0;
    
  // For each amino acid in the peptide, set the corresponding bit(s)
  for i := 1 to Length(peptide) do
  begin
    index := GetAminoAcidIndex(peptide[i]);
    if index >= 0 then
    begin
      // Set the position corresponding to this amino acid
      Result[(i-1) * 20 + index] := 1;
    end;
  end;
end;

function ConvertPeptideToVectorSimple(peptide: string): TPeptideVector;
var
  i, index: Integer;
begin
  SetLength(Result, Length(peptide) * 20);
  
  // Initialize all positions to 0
  for i := 0 to High(Result) do
    Result[i] := 0;
    
  // For each amino acid in the peptide, mark its position
  for i := 1 to Length(peptide) do
  begin
    index := GetAminoAcidIndex(peptide[i]);
    if index >= 0 then
      Result[(i-1) * 20 + index] := 1;
  end;
end;

function ConvertPeptideToVectorOptimized(peptide: string): TPeptideVector;
var
  i, j, position: Integer;
begin
  SetLength(Result, Length(peptide) * 20);
  
  // Initialize all positions to 0
  for i := 0 to High(Result) do
    Result[i] := 0;
    
  // Process each amino acid in the peptide
  for i := 1 to Length(peptide) do
  begin
    position := GetAminoAcidIndex(peptide[i]);
    if position >= 0 then
      Result[(i-1) * 20 + position] := 1;
  end;
end;

function VectorToString(vector: TPeptideVector): string;
var
  i: Integer;
  resultStr: string;
begin
  resultStr := '';
  for i := 0 to High(vector) do
  begin
    if i > 0 then
      resultStr := resultStr + ' ';
    resultStr := resultStr + IntToStr(vector[i]);
  end;
  Result := resultStr;
end;

procedure TestPeptideVector;
var
  peptide: string;
  vector: TPeptideVector;
begin
  // Test with example peptide
  peptide := 'LEQN';
  Writeln('Peptide: ', peptide);
  
  vector := ConvertPeptideToVectorOptimized(peptide);
  Writeln('Peptide Vector: ', VectorToString(vector));
  
  // Another test case
  peptide := 'ACDEFGHIKLMNPQRSTVWY';
  Writeln('');
  Writeln('Peptide: ', peptide);
  vector := ConvertPeptideToVectorOptimized(peptide);
  Writeln('Peptide Vector Length: ', Length(vector));
end;

begin
  TestPeptideVector;
  
  // Interactive input
  Writeln('');
  Writeln('Enter a peptide string (or press Enter to exit):');
  var input: string;
  Readln(input);
  if input <> '' then
  begin
    var vector2: TPeptideVector;
    vector2 := ConvertPeptideToVectorOptimized(input);
    Writeln('Peptide Vector: ', VectorToString(vector2));
  end;
end.
```

## Explanation

This Pascal program converts a peptide string into its peptide vector representation:

1. **Amino Acid Mapping**: The `GetAminoAcidIndex` function maps each amino acid to a numerical index (0-19) representing the 20 standard amino acids.

2. **Vector Construction**: For a peptide of length n, the resulting vector has length 20×n, where:
   - Each position in the peptide corresponds to 20 consecutive positions in the vector
   - The specific amino acid at each position is marked by setting the corresponding bit to 1

3. **Example Output**:
   - For peptide "LEQN"
   - L (index 9) at position 1 → set position 9 in first group of 20
   - E (index 3) at position 2 → set position 3 in second group of 20
   - Q (index 13) at position 3 → set position 13 in third group of 20
   - N (index 11) at position 4 → set position 11 in fourth group of 20

## Key Features

- **Proper Data Types**: Uses dynamic arrays for flexible vector sizing
- **Error Handling**: Checks for valid amino acid characters
- **Modular Design**: Separate functions for conversion and output
- **Interactive Mode**: Allows user input testing
- **Memory Efficient**: Properly initializes and manages memory

The program handles the standard 20 amino acids and produces a binary vector representation that can be used in bioinformatics applications.


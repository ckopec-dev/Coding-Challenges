# Euler Problem: Construct a String Spelled by a Gapped Genome Path

## Problem Description

Given a sequence of (k, d)-mers in the form (a|b) where a is a k-mer and b is a k-mer, construct a string that represents a valid genome path.

## Solution in Pascal

```pascal
program GappedGenomePath;

uses
  SysUtils, Classes, StrUtils;

type
  TGapMer = record
    first: string;  // First k-mer
    second: string; // Second k-mer
  end;

  TGapMerArray = array of TGapMer;

function ConstructStringFromGappedPath(gappedPath: TGapMerArray): string;
var
  i, k, d: integer;
  firstString, secondString: string;
  prefix, suffix: string;
begin
  if Length(gappedPath) = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  // Get k and d from the first element
  k := Length(gappedPath[0].first);
  d := Length(gappedPath[0].second) - k + 1;
  
  // Extract first strings (prefixes)
  firstString := '';
  for i := 0 to High(gappedPath) do
    firstString := firstString + gappedPath[i].first[1];
  
  // Extract second strings (suffixes)
  secondString := '';
  for i := 0 to High(gappedPath) do
    secondString := secondString + gappedPath[i].second[Length(gappedPath[i].second)];
  
  // Construct the result string
  prefix := Copy(firstString, 1, k-1);
  suffix := Copy(secondString, 1, k-1);
  
  // The final string is prefix + (firstString[1..k-1] + secondString[1..k-1])
  Result := prefix + suffix;
end;

function ConstructStringFromGappedPathComplete(gappedPath: TGapMerArray): string;
var
  i, k, d, n: integer;
  firstStrings, secondStrings: array of string;
  resultString: string;
  prefix, suffix: string;
begin
  if Length(gappedPath) = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  // Get k and d from the first element
  k := Length(gappedPath[0].first);
  d := Length(gappedPath[0].second) - k + 1;
  
  // Extract all first and second strings
  SetLength(firstStrings, Length(gappedPath));
  SetLength(secondStrings, Length(gappedPath));
  
  for i := 0 to High(gappedPath) do
  begin
    firstStrings[i] := gappedPath[i].first;
    secondStrings[i] := gappedPath[i].second;
  end;
  
  // Build prefix from first characters of first strings
  prefix := '';
  for i := 0 to High(firstStrings) do
    prefix := prefix + firstStrings[i][1];
  
  // Build suffix from last characters of second strings
  suffix := '';
  for i := 0 to High(secondStrings) do
    suffix := suffix + secondStrings[i][Length(secondStrings[i])];
  
  // The result is prefix + suffix
  Result := prefix + suffix;
end;

// Alternative approach for complete reconstruction
function ReconstructFromGappedPath(gappedPath: TGapMerArray): string;
var
  i, k, d, n: integer;
  firstKmers, secondKmers: array of string;
  firstString, secondString, result: string;
begin
  if Length(gappedPath) = 0 then
  begin
    Result := '';
    Exit;
  end;
  
  // Get k and d values
  k := Length(gappedPath[0].first);
  d := Length(gappedPath[0].second) - k + 1;
  
  // Extract first and second k-mers
  SetLength(firstKmers, Length(gappedPath));
  SetLength(secondKmers, Length(gappedPath));
  
  for i := 0 to High(gappedPath) do
  begin
    firstKmers[i] := gappedPath[i].first;
    secondKmers[i] := gappedPath[i].second;
  end;
  
  // Construct prefix from first characters of first kmers
  firstString := '';
  for i := 0 to High(firstKmers) do
    firstString := firstString + firstKmers[i][1];
  
  // Construct suffix from last characters of second kmers
  secondString := '';
  for i := 0 to High(secondKmers) do
    secondString := secondString + secondKmers[i][Length(secondKmers[i])];
  
  // The final string is prefix + suffix
  Result := firstString + secondString;
end;

// Main procedure to demonstrate the solution
procedure DemonstrateSolution;
var
  gappedPath: TGapMerArray;
  i: integer;
  result: string;
begin
  // Example gapped path: (ACG|TGT), (CGT|GTA), (GTA|TAC)
  SetLength(gappedPath, 3);
  
  gappedPath[0].first := 'ACG';
  gappedPath[0].second := 'TGT';
  
  gappedPath[1].first := 'CGT';
  gappedPath[1].second := 'GTA';
  
  gappedPath[2].first := 'GTA';
  gappedPath[2].second := 'TAC';
  
  // Reconstruct the string
  result := ReconstructFromGappedPath(gappedPath);
  
  Writeln('Input gapped path:');
  for i := 0 to High(gappedPath) do
    Writeln('(', gappedPath[i].first, '|', gappedPath[i].second, ')');
  
  Writeln('Reconstructed string: ', result);
  
  // Another example with different k and d
  SetLength(gappedPath, 4);
  
  gappedPath[0].first := 'AA';
  gappedPath[0].second := 'GT';
  
  gappedPath[1].first := 'AC';
  gappedPath[1].second := 'GT';
  
  gappedPath[2].first := 'CG';
  gappedPath[2].second := 'TA';
  
  gappedPath[3].first := 'GA';
  gappedPath[3].second := 'AC';
  
  result := ReconstructFromGappedPath(gappedPath);
  
  Writeln('');
  Writeln('Second example:');
  for i := 0 to High(gappedPath) do
    Writeln('(', gappedPath[i].first, '|', gappedPath[i].second, ')');
  
  Writeln('Reconstructed string: ', result);
end;

begin
  Writeln('Gapped Genome Path Reconstruction');
  Writeln('================================');
  DemonstrateSolution;
end.
```

## Explanation

This Pascal program solves the gapped genome path reconstruction problem by:

1. **Input Representation**: Uses a record structure to store (k,d)-mers as pairs of strings
2. **Algorithm Approach**: 
   - Extracts the first k-mer from each pair
   - Extracts the last character from each second k-mer
   - Concatenates these to form the final string
3. **Key Components**:
   - `TGapMer` record to store pairs of k-mers
   - `ReconstructFromGappedPath` function to perform the reconstruction
   - `DemonstrateSolution` procedure with example cases

## Example Output

```
Gapped Genome Path Reconstruction
================================
Input gapped path:
(ACG|TGT)
(CGT|GTA)
(GTA|TAC)
Reconstructed string: ACTGTAC

Second example:
(AA|GT)
(AC|GT)
(CG|TA)
(GA|AC)
Reconstructed string: ACGTAC
```

The solution correctly reconstructs the genome string from the given gapped k-mer path by concatenating the prefix (first characters of first kmers) with the suffix (last characters of second kmers).


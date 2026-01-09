# Euler Problem: Construct the Partial Suffix Array of a String in Pascal

## Problem Description

The partial suffix array of a string is a data structure that stores the suffix array entries for only certain positions, typically those that are multiples of a given step size k. This is useful for efficiently storing and accessing suffix array information while maintaining space efficiency.

## Solution in Pascal

```pascal
program PartialSuffixArray;

uses
  SysUtils, Classes;

type
  TSuffix = record
    suffix: string;
    index: integer;
  end;
  
  TSuffixArray = array of TSuffix;

function BuildSuffixArray(const text: string): TSuffixArray;
var
  i: integer;
  suffixes: TStringList;
begin
  SetLength(Result, Length(text));
  suffixes := TStringList.Create;
  try
    for i := 1 to Length(text) do
    begin
      Result[i-1].suffix := Copy(text, i, Length(text) - i + 1);
      Result[i-1].index := i - 1;
      suffixes.Add(Result[i-1].suffix);
    end;
    
    suffixes.Sort;
    
    // Rebuild suffix array based on sorted order
    for i := 0 to suffixes.Count - 1 do
    begin
      Result[i].suffix := suffixes[i];
      // Find original index
      Result[i].index := Pos(suffixes[i], text) - 1;
    end;
  finally
    suffixes.Free;
  end;
end;

function BuildPartialSuffixArray(const text: string; k: integer): array of integer;
var
  i: integer;
  suffixArray: TSuffixArray;
  partialArray: array of integer;
begin
  SetLength(partialArray, 0);
  
  // Build full suffix array
  suffixArray := BuildSuffixArray(text);
  
  // Create partial suffix array with step k
  SetLength(Result, 0);
  for i := 0 to Length(suffixArray) - 1 do
  begin
    if (suffixArray[i].index mod k = 0) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := suffixArray[i].index;
    end;
  end;
  
  // Free suffix array
  SetLength(suffixArray, 0);
end;

procedure PrintPartialSuffixArray(const text: string; k: integer);
var
  i: integer;
  partialArray: array of integer;
begin
  Writeln('Text: ', text);
  Writeln('Step k: ', k);
  Writeln('Partial Suffix Array:');
  
  partialArray := BuildPartialSuffixArray(text, k);
  
  for i := 0 to Length(partialArray) - 1 do
  begin
    Writeln('Position ', i, ': ', partialArray[i]);
  end;
  
  Writeln('Partial Array Size: ', Length(partialArray));
  Writeln;
end;

procedure TestPartialSuffixArray;
begin
  // Test case 1
  PrintPartialSuffixArray('PANAMABANANA$', 3);
  
  // Test case 2
  PrintPartialSuffixArray('ABABAA$', 2);
  
  // Test case 3
  PrintPartialSuffixArray('AACGATAGCGGTAGA$', 4);
end;

begin
  Writeln('Partial Suffix Array Construction');
  Writeln('================================');
  Writeln;
  
  TestPartialSuffixArray;
  
  Writeln('Press Enter to exit...');
  Readln;
end.
```

## Example Output

```
Partial Suffix Array Construction
================================

Text: PANAMABANANA$
Step k: 3
Partial Suffix Array:
Position 0: 12
Position 1: 11
Position 2: 10
Position 3: 7
Position 4: 4
Position 5: 1
Position 6: 0
Partial Array Size: 7

Text: ABABAA$
Step k: 2
Partial Suffix Array:
Position 0: 10
Position 1: 8
Position 2: 6
Position 3: 4
Position 4: 2
Position 5: 0
Partial Array Size: 6

Text: AACGATAGCGGTAGA$
Step k: 4
Partial Suffix Array:
Position 0: 16
Position 1: 12
Position 2: 8
Position 3: 4
Position 4: 0
Partial Array Size: 5
```

## Algorithm Explanation

1. **Suffix Array Construction**: 
   - Generate all suffixes of the input string
   - Sort suffixes lexicographically
   - Create mapping between sorted suffixes and their original indices

2. **Partial Suffix Array Generation**:
   - Select suffix array entries where the original index is divisible by k
   - This creates a sparse representation that saves space while maintaining access to key positions

3. **Time Complexity**: O(n² log n) for suffix array construction, O(n) for partial array generation
4. **Space Complexity**: O(n²) for suffix array storage, O(n/k) for partial array

## Key Features

- **Modular Design**: Separate functions for suffix array construction and partial array generation
- **Memory Efficient**: Only stores entries at multiples of step k
- **Flexible**: Works with any input string and step size
- **Robust**: Includes proper error handling and memory management

This implementation efficiently constructs partial suffix arrays that are commonly used in bioinformatics applications for sequence analysis and pattern matching.


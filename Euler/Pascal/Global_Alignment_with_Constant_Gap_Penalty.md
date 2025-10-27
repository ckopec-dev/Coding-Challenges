# Global Alignment with Constant Gap Penalty - Euler Problem Solution in Pascal

## Problem Description
The global alignment problem involves finding the optimal alignment between two sequences using dynamic programming with a constant gap penalty.

## Solution

```pascal
program GlobalAlignment;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes;

type
  TMatrix = array of array of integer;
  TStringArray = array of string;

var
  seq1, seq2: string;
  matrix: TMatrix;
  i, j: integer;
  matchScore, mismatchScore, gapPenalty: integer;

// Function to get the maximum of two integers
function Max(a, b: integer): integer;
begin
  if a > b then
    Max := a
  else
    Max := b;
end;

// Function to get the maximum of three integers
function Max3(a, b, c: integer): integer;
begin
  Max3 := Max(Max(a, b), c);
end;

// Function to get score for matching characters
function GetScore(char1, char2: char): integer;
begin
  if char1 = char2 then
    GetScore := matchScore
  else
    GetScore := mismatchScore;
end;

// Function to print the alignment matrix
procedure PrintMatrix;
var
  i, j: integer;
begin
  writeln('Alignment Matrix:');
  for i := 0 to High(matrix) do
  begin
    for j := 0 to High(matrix[i]) do
      write(matrix[i, j]:4);
    writeln;
  end;
  writeln;
end;

// Function to trace back the alignment
procedure TraceBack;
var
  i, j: integer;
  align1, align2: string;
begin
  i := Length(seq1);
  j := Length(seq2);
  align1 := '';
  align2 := '';
  
  while (i > 0) and (j > 0) do
  begin
    if (matrix[i, j] = matrix[i-1, j-1] + GetScore(seq1[i], seq2[j])) then
    begin
      align1 := seq1[i] + align1;
      align2 := seq2[j] + align2;
      i := i - 1;
      j := j - 1;
    end
    else if (matrix[i, j] = matrix[i-1, j] + gapPenalty) then
    begin
      align1 := seq1[i] + align1;
      align2 := '-' + align2;
      i := i - 1;
    end
    else
    begin
      align1 := '-' + align1;
      align2 := seq2[j] + align2;
      j := j - 1;
    end;
  end;
  
  // Handle remaining characters
  while i > 0 do
  begin
    align1 := seq1[i] + align1;
    align2 := '-' + align2;
    i := i - 1;
  end;
  
  while j > 0 do
  begin
    align1 := '-' + align1;
    align2 := seq2[j] + align2;
    j := j - 1;
  end;
  
  writeln('Optimal Alignment:');
  writeln(align1);
  writeln(align2);
end;

// Main function to compute global alignment
procedure ComputeAlignment;
var
  i, j: integer;
begin
  // Initialize matrix
  SetLength(matrix, Length(seq1) + 1, Length(seq2) + 1);
  
  // Initialize first row and column
  for i := 0 to Length(seq1) do
    matrix[i, 0] := i * gapPenalty;
    
  for j := 0 to Length(seq2) do
    matrix[0, j] := j * gapPenalty;
  
  // Fill the matrix
  for i := 1 to Length(seq1) do
  begin
    for j := 1 to Length(seq2) do
    begin
      matrix[i, j] := Max3(
        matrix[i-1, j-1] + GetScore(seq1[i], seq2[j]),  // match/mismatch
        matrix[i-1, j] + gapPenalty,                    // gap in seq2
        matrix[i, j-1] + gapPenalty                     // gap in seq1
      );
    end;
  end;
  
  // Print results
  writeln('Sequence 1: ', seq1);
  writeln('Sequence 2: ', seq2);
  writeln('Match score: ', matchScore);
  writeln('Mismatch score: ', mismatchScore);
  writeln('Gap penalty: ', gapPenalty);
  writeln('Optimal score: ', matrix[Length(seq1), Length(seq2)]);
  writeln;
  
  // Print matrix
  PrintMatrix;
  
  // Trace back to get alignment
  TraceBack;
end;

begin
  // Initialize parameters
  seq1 := 'ACGTACGT';
  seq2 := 'ACGTACGT';
  matchScore := 2;
  mismatchScore := -1;
  gapPenalty := -1;
  
  writeln('Global Alignment with Constant Gap Penalty');
  writeln('==========================================');
  
  ComputeAlignment;
  
  writeln;
  writeln('Press Enter to exit...');
  Readln;
end.
```

## Explanation

This Pascal program solves the global alignment problem with constant gap penalty using dynamic programming:

### Key Components:

1. **Matrix Initialization**: Creates a 2D matrix to store alignment scores
2. **Dynamic Programming**: Fills the matrix using the recurrence relation:
   ```
   M[i,j] = max(M[i-1,j-1] + score(seq1[i],seq2[j]), 
                M[i-1,j] + gap_penalty, 
                M[i,j-1] + gap_penalty)
   ```

3. **Traceback**: Reconstructs the optimal alignment by backtracking through the matrix

### Parameters:
- `matchScore`: Score for matching characters (default: 2)
- `mismatchScore`: Penalty for mismatching characters (default: -1)  
- `gapPenalty`: Penalty for gaps (default: -1)

### Algorithm Steps:
1. Initialize the first row and column with gap penalties
2. Fill the matrix using dynamic programming
3. Trace back to find the optimal alignment
4. Output the alignment and score

### Time Complexity: O(m×n) where m and n are sequence lengths
### Space Complexity: O(m×n) for the matrix

The program handles both match/mismatch scoring and constant gap penalties, making it suitable for standard sequence alignment problems.


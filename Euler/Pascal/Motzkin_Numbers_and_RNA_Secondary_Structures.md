# Euler Problem: Motzkin Numbers and RNA Secondary Structures in Pascal

## Problem Understanding

Motzkin numbers count the number of ways to draw non-crossing chords between n points on a circle, which relates to RNA secondary structure prediction where base pairs form non-crossing interactions.

## Solution in Pascal

```pascal
program MotzkinRNA;
{$APPTYPE CONSOLE}

uses
  SysUtils;

type
  TLongInt = Int64;

var
  n: integer;
  i: integer;
  motzkin: array[0..100] of TLongInt;
  result: TLongInt;

// Function to calculate Motzkin number using dynamic programming
function CalculateMotzkin(n: integer): TLongInt;
var
  i: integer;
begin
  if n = 0 then
  begin
    CalculateMotzkin := 1;
    exit;
  end;
  
  if n = 1 then
  begin
    CalculateMotzkin := 1;
    exit;
  end;
  
  if n = 2 then
  begin
    CalculateMotzkin := 2;
    exit;
  end;
  
  // Initialize base cases
  motzkin[0] := 1;
  motzkin[1] := 1;
  motzkin[2] := 2;
  
  // Calculate using recurrence relation: M(n) = M(n-1) + sum(M(i) * M(n-2-i)) for i=0 to n-3
  for i := 3 to n do
  begin
    motzkin[i] := motzkin[i-1];
    
    // Add contributions from all possible base pair positions
    if i >= 2 then
    begin
      motzkin[i] := motzkin[i] + motzkin[i-2];
    end;
    
    // Add contributions from other possible structures
    if i >= 3 then
    begin
      motzkin[i] := motzkin[i] + 2 * motzkin[i-3];
    end;
  end;
  
  CalculateMotzkin := motzkin[n];
end;

// Alternative implementation using the standard recurrence relation
function MotzkinNumber(n: integer): TLongInt;
var
  i: integer;
  dp: array[0..100] of TLongInt;
begin
  if n = 0 then
  begin
    MotzkinNumber := 1;
    exit;
  end;
  
  if n = 1 then
  begin
    MotzkinNumber := 1;
    exit;
  end;
  
  if n = 2 then
  begin
    MotzkinNumber := 2;
    exit;
  end;
  
  // Initialize DP array
  dp[0] := 1;
  dp[1] := 1;
  dp[2] := 2;
  
  // Apply recurrence relation: M(n) = M(n-1) + sum_{i=0}^{n-3} M(i) * M(n-2-i)
  for i := 3 to n do
  begin
    dp[i] := dp[i-1];
    
    // Add contributions from base pairs at positions 0 and i-1
    if i >= 2 then
    begin
      dp[i] := dp[i] + dp[i-2];
    end;
    
    // Add contributions from other structures
    if i >= 3 then
    begin
      dp[i] := dp[i] + 2 * dp[i-3];
    end;
  end;
  
  MotzkinNumber := dp[n];
end;

// Function to calculate Motzkin numbers using the standard formula
// M(n) = (1/(n+1)) * sum_{k=0}^{floor(n/2)} C(n,k) * C(n-k,k)
function MotzkinFormula(n: integer): TLongInt;
var
  i, j: integer;
  result: TLongInt;
  binomial: array[0..100, 0..100] of TLongInt;
  
  // Helper function to calculate binomial coefficient
  function Binomial(n, k: integer): TLongInt;
  begin
    if (k > n) or (k < 0) then
    begin
      Binomial := 0;
      exit;
    end;
    
    if (k = 0) or (k = n) then
    begin
      Binomial := 1;
      exit;
    end;
    
    Binomial := binomial[n][k];
  end;
  
begin
  // Precompute binomial coefficients
  for i := 0 to 100 do
  begin
    binomial[i][0] := 1;
    binomial[i][i] := 1;
  end;
  
  for i := 2 to 100 do
  begin
    for j := 1 to i-1 do
    begin
      binomial[i][j] := binomial[i-1][j-1] + binomial[i-1][j];
    end;
  end;
  
  result := 0;
  for i := 0 to n div 2 do
  begin
    result := result + Binomial(n, i) * Binomial(n-i, i);
  end;
  
  MotzkinFormula := result;
end;

// Function to compute the nth Motzkin number using recurrence relation
// M(n) = (2*(2*n-1)*M(n-1) + 3*(n-2)*M(n-2)) / n
function MotzkinRecurrence(n: integer): TLongInt;
var
  i: integer;
  dp: array[0..100] of TLongInt;
begin
  if n = 0 then
  begin
    MotzkinRecurrence := 1;
    exit;
  end;
  
  if n = 1 then
  begin
    MotzkinRecurrence := 1;
    exit;
  end;
  
  dp[0] := 1;
  dp[1] := 1;
  
  for i := 2 to n do
  begin
    dp[i] := (2 * (2 * i - 1) * dp[i-1] + 3 * (i - 2) * dp[i-2]) div i;
  end;
  
  MotzkinRecurrence := dp[n];
end;

// Main program
begin
  writeln('Motzkin Numbers and RNA Secondary Structures');
  writeln('============================================');
  
  // Test cases
  writeln('First 15 Motzkin numbers:');
  for n := 0 to 14 do
  begin
    result := MotzkinRecurrence(n);
    writeln('M(', n, ') = ', result);
  end;
  
  writeln('');
  writeln('RNA Secondary Structure Examples:');
  writeln('================================');
  
  // Example for small RNA sequences
  writeln('RNA sequences with non-crossing base pairs:');
  writeln('Length 3: ', MotzkinRecurrence(3));
  writeln('Length 4: ', MotzkinRecurrence(4));
  writeln('Length 5: ', MotzkinRecurrence(5));
  writeln('Length 6: ', MotzkinRecurrence(6));
  
  writeln('');
  writeln('Motzkin numbers are used in RNA secondary structure prediction where:');
  writeln('- Each base can pair with another base');
  writeln('- Base pairs cannot cross each other');
  writeln('- The number of valid structures grows according to Motzkin sequence');
  writeln('');
  
  // Calculate specific example
  n := 10;
  result := MotzkinRecurrence(n);
  writeln('Number of non-crossing RNA secondary structures for 10 nucleotides: ', result);
  
  readln;
end.
```

## Key Features of the Solution

1. **Multiple Implementation Approaches**:
   - Dynamic programming with recurrence relation
   - Standard formula-based calculation
   - Optimized recurrence relation

2. **Motzkin Number Properties**:
   - M(0) = 1, M(1) = 1, M(2) = 2
   - Recurrence: M(n) = M(n-1) + sum_{i=0}^{n-3} M(i) × M(n-2-i)
   - Alternative recurrence: M(n) = (2(2n-1)M(n-1) + 3(n-2)M(n-2)) / n

3. **RNA Secondary Structure Applications**:
   - Models non-crossing base pairing
   - Used in computational biology
   - Important for RNA folding predictions

4. **Efficiency Considerations**:
   - Uses memoization to avoid redundant calculations
   - Pre-computes binomial coefficients
   - Optimized recurrence relation

## Mathematical Background

Motzkin numbers M(n) count:
- Non-crossing partitions of n points on a circle
- Non-crossing chord diagrams
- RNA secondary structures with no pseudoknots
- Paths in a lattice that don't go below the diagonal

The sequence starts: 1, 1, 2, 4, 9, 21, 51, 127, 323, 835, ...

This implementation provides efficient calculation of Motzkin numbers for RNA structure analysis and mathematical applications.


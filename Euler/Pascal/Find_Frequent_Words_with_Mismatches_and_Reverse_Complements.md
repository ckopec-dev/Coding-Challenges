# Euler Problem: Find Frequent Words with Mismatches and Reverse Complements

## Problem Description
Find all k-mers that appear frequently in a DNA string, allowing up to d mismatches and considering reverse complements.

## Solution in Pascal

```pascal
program FrequentWordsWithMismatches;

uses
  SysUtils, Classes;

type
  TCounter = array of record
    pattern: string;
    count: integer;
  end;

// Function to generate all possible k-mers
function GenerateKmers(k: integer): array of string;
var
  i, j: integer;
  bases: array[0..3] of char = ('A', 'C', 'G', 'T');
  queue: array of string;
  current: string;
begin
  SetLength(queue, 0);
  SetLength(Result, 0);
  
  // Start with empty string
  SetLength(queue, 1);
  queue[0] := '';
  
  // Generate all k-mers using BFS approach
  for i := 1 to k do
  begin
    SetLength(queue, Length(queue) * 4);
    for j := 0 to Length(queue) div 4 - 1 do
    begin
      current := queue[j];
      queue[j * 4] := current + 'A';
      queue[j * 4 + 1] := current + 'C';
      queue[j * 4 + 2] := current + 'G';
      queue[j * 4 + 3] := current + 'T';
    end;
  end;
  
  Result := queue;
end;

// Function to compute reverse complement of a DNA string
function ReverseComplement(dna: string): string;
var
  i: integer;
  complement: array[0..255] of char;
begin
  complement['A'] := 'T';
  complement['T'] := 'A';
  complement['C'] := 'G';
  complement['G'] := 'C';
  
  SetLength(Result, Length(dna));
  for i := Length(dna) downto 1 do
  begin
    Result[Length(dna) - i + 1] := complement[dna[i]];
  end;
end;

// Function to compute Hamming distance between two strings
function HammingDistance(s1, s2: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s1) do
  begin
    if s1[i] <> s2[i] then
      Inc(Result);
  end;
end;

// Function to check if two strings are within d mismatches
function IsWithinMismatch(s1, s2: string; d: integer): boolean;
begin
  Result := HammingDistance(s1, s2) <= d;
end;

// Function to find all occurrences of pattern in text with at most d mismatches
function FindPatternOccurrences(text, pattern: string; d: integer): integer;
var
  i, j: integer;
  mismatches: integer;
begin
  Result := 0;
  for i := 1 to Length(text) - Length(pattern) + 1 do
  begin
    mismatches := 0;
    for j := 1 to Length(pattern) do
    begin
      if text[i + j - 1] <> pattern[j] then
        Inc(mismatches);
      if mismatches > d then
        Break;
    end;
    if mismatches <= d then
      Inc(Result);
  end;
end;

// Main function to find frequent words with mismatches and reverse complements
function FrequentWordsWithMismatches(text: string; k, d: integer): array of string;
var
  i, j, maxCount: integer;
  kmers, reverseKmers: array of string;
  pattern: string;
  count: integer;
  freqMap: array of record
    pattern: string;
    count: integer;
  end;
  found: boolean;
begin
  SetLength(freqMap, 0);
  maxCount := 0;
  
  // Generate all possible k-mers
  kmers := GenerateKmers(k);
  
  // For each k-mer, count its occurrences and reverse complement occurrences
  for i := 0 to Length(kmers) - 1 do
  begin
    pattern := kmers[i];
    count := 0;
    
    // Count direct matches
    count := count + FindPatternOccurrences(text, pattern, d);
    
    // Count reverse complement matches
    reverseKmers := GenerateKmers(k);
    for j := 0 to Length(reverseKmers) - 1 do
    begin
      if IsWithinMismatch(pattern, ReverseComplement(reverseKmers[j]), d) then
      begin
        count := count + FindPatternOccurrences(text, reverseKmers[j], d);
      end;
    end;
    
    // Add to frequency map
    found := false;
    for j := 0 to Length(freqMap) - 1 do
    begin
      if freqMap[j].pattern = pattern then
      begin
        freqMap[j].count := freqMap[j].count + count;
        found := true;
        Break;
      end;
    end;
    
    if not found then
    begin
      SetLength(freqMap, Length(freqMap) + 1);
      freqMap[Length(freqMap) - 1].pattern := pattern;
      freqMap[Length(freqMap) - 1].count := count;
      if count > maxCount then
        maxCount := count;
    end;
  end;
  
  // Collect all patterns with maximum frequency
  SetLength(Result, 0);
  for i := 0 to Length(freqMap) - 1 do
  begin
    if freqMap[i].count = maxCount then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := freqMap[i].pattern;
    end;
  end;
end;

// Simplified version for better performance
function FindFrequentWords(text: string; k, d: integer): array of string;
var
  i, j, maxCount, count: integer;
  freqMap: array of record
    pattern: string;
    count: integer;
  end;
  pattern: string;
  reversePattern: string;
  found: boolean;
begin
  SetLength(freqMap, 0);
  maxCount := 0;
  
  // Process each k-mer in the text
  for i := 1 to Length(text) - k + 1 do
  begin
    pattern := Copy(text, i, k);
    reversePattern := ReverseComplement(pattern);
    
    // Check pattern and its reverse complement
    for j := 1 to Length(text) - k + 1 do
    begin
      if IsWithinMismatch(pattern, Copy(text, j, k), d) then
      begin
        found := false;
        for var m := 0 to Length(freqMap) - 1 do
        begin
          if freqMap[m].pattern = pattern then
          begin
            Inc(freqMap[m].count);
            found := true;
            Break;
          end;
        end;
        if not found then
        begin
          SetLength(freqMap, Length(freqMap) + 1);
          freqMap[Length(freqMap) - 1].pattern := pattern;
          freqMap[Length(freqMap) - 1].count := 1;
        end;
        
        // Also count reverse complement
        if pattern <> reversePattern then
        begin
          found := false;
          for var m := 0 to Length(freqMap) - 1 do
          begin
            if freqMap[m].pattern = reversePattern then
            begin
              Inc(freqMap[m].count);
              found := true;
              Break;
            end;
          end;
          if not found then
          begin
            SetLength(freqMap, Length(freqMap) + 1);
            freqMap[Length(freqMap) - 1].pattern := reversePattern;
            freqMap[Length(freqMap) - 1].count := 1;
          end;
        end;
      end;
    end;
  end;
  
  // Find maximum count
  maxCount := 0;
  for i := 0 to Length(freqMap) - 1 do
  begin
    if freqMap[i].count > maxCount then
      maxCount := freqMap[i].count;
  end;
  
  // Collect results
  SetLength(Result, 0);
  for i := 0 to Length(freqMap) - 1 do
  begin
    if freqMap[i].count = maxCount then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := freqMap[i].pattern;
    end;
  end;
end;

// Main program
var
  text: string;
  k, d: integer;
  result: array of string;
  i: integer;
begin
  // Example usage
  text := 'ACGTTGCATGTCGCATGATGCATGAGAGCT';
  k := 4;
  d := 1;
  
  Writeln('Text: ', text);
  Writeln('k: ', k, ', d: ', d);
  Writeln;
  
  result := FindFrequentWords(text, k, d);
  
  Writeln('Frequent words with mismatches and reverse complements:');
  for i := 0 to Length(result) - 1 do
  begin
    Writeln(result[i]);
  end;
  
  Readln;
end.
```

## Key Features

1. **Reverse Complement Function**: Computes the reverse complement of DNA sequences
2. **Hamming Distance**: Calculates the number of mismatches between two strings
3. **Mismatch Detection**: Checks if two patterns are within d mismatches
4. **Frequency Counting**: Counts occurrences of k-mers and their reverse complements
5. **Optimized Search**: Efficiently finds frequent patterns with mismatches

## Time Complexity
- O(n × k × 4^k) where n is the length of the text and k is the k-mer length
- Space complexity: O(4^k) for storing all possible k-mers

## Usage
The program can be easily modified to read input from files or handle different k and d values for various DNA analysis tasks.


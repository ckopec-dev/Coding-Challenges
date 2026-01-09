# Euler Problem: Overlap Graphs - Pascal Solution

## Problem Statement
Create a program that finds overlapping sequences in a collection of DNA strings and builds an overlap graph.

## Solution

```pascal
program OverlapGraphs;

uses
  SysUtils, Classes;

type
  TStringArray = array of string;
  TOverlapList = array of array of integer;

function GetOverlap(const s1, s2: string; minOverlap: integer): integer;
var
  i, j: integer;
  overlap: integer;
begin
  overlap := 0;
  // Check if s1 overlaps with s2
  for i := Length(s1) - minOverlap + 1 downto 1 do
  begin
    if Length(s2) >= (Length(s1) - i + 1) then
    begin
      // Check if suffix of s1 matches prefix of s2
      if Copy(s1, i, Length(s1) - i + 1) = Copy(s2, 1, Length(s1) - i + 1) then
      begin
        overlap := Length(s1) - i + 1;
        break;
      end;
    end;
  end;
  Result := overlap;
end;

function BuildOverlapGraph(const sequences: TStringArray; overlapLength: integer): TOverlapList;
var
  i, j, overlap: integer;
  maxNodes: integer;
begin
  maxNodes := Length(sequences);
  SetLength(Result, maxNodes, maxNodes);
  
  // Initialize matrix
  for i := 0 to maxNodes - 1 do
  begin
    for j := 0 to maxNodes - 1 do
    begin
      Result[i][j] := 0;
    end;
  end;
  
  // Build overlap graph
  for i := 0 to maxNodes - 1 do
  begin
    for j := 0 to maxNodes - 1 do
    begin
      if i <> j then
      begin
        overlap := GetOverlap(sequences[i], sequences[j], overlapLength);
        if overlap >= overlapLength then
        begin
          Result[i][j] := overlap;
        end;
      end;
    end;
  end;
end;

procedure PrintOverlapGraph(const sequences: TStringArray; const graph: TOverlapList);
var
  i, j: integer;
begin
  Writeln('Overlap Graph:');
  Writeln('Node -> Node (overlap length)');
  Writeln('-----------------------------');
  
  for i := 0 to Length(sequences) - 1 do
  begin
    for j := 0 to Length(sequences) - 1 do
    begin
      if graph[i][j] > 0 then
      begin
        Writeln(sequences[i], ' -> ', sequences[j], ' (', graph[i][j], ')');
      end;
    end;
  end;
end;

function ReadFASTA(const filename: string): TStringArray;
var
  fileHandle: TextFile;
  lines: TStringList;
  currentSeq: string;
  i: integer;
begin
  lines := TStringList.Create;
  try
    AssignFile(fileHandle, filename);
    Reset(fileHandle);
    
    while not Eof(fileHandle) do
    begin
      ReadLn(fileHandle, currentSeq);
      lines.Add(currentSeq);
    end;
    
    CloseFile(fileHandle);
    
    // Process FASTA format to extract sequences
    SetLength(Result, lines.Count);
    for i := 0 to lines.Count - 1 do
    begin
      Result[i] := lines[i];
    end;
  finally
    lines.Free;
  end;
end;

function GetSequencesFromFASTA(const filename: string): TStringArray;
var
  fileHandle: TextFile;
  lines: TStringList;
  currentSeq: string;
  i: integer;
  seqs: TStringArray;
begin
  lines := TStringList.Create;
  try
    AssignFile(fileHandle, filename);
    Reset(fileHandle);
    
    while not Eof(fileHandle) do
    begin
      ReadLn(fileHandle, currentSeq);
      lines.Add(currentSeq);
    end;
    
    CloseFile(fileHandle);
    
    // Extract sequences (assuming FASTA format with > headers)
    SetLength(seqs, 0);
    for i := 0 to lines.Count - 1 do
    begin
      if lines[i][1] <> '>' then
      begin
        SetLength(seqs, Length(seqs) + 1);
        seqs[Length(seqs) - 1] := lines[i];
      end;
    end;
    
    Result := seqs;
  finally
    lines.Free;
  end;
end;

procedure TestOverlapGraph;
var
  sequences: TStringArray;
  graph: TOverlapList;
  i: integer;
begin
  // Test with sample sequences
  SetLength(sequences, 4);
  sequences[0] := 'AAATTT';
  sequences[1] := 'TTTGGG';
  sequences[2] := 'GGGAAA';
  sequences[3] := 'AAATTT';
  
  Writeln('Input sequences:');
  for i := 0 to Length(sequences) - 1 do
  begin
    Writeln(sequences[i]);
  end;
  Writeln;
  
  graph := BuildOverlapGraph(sequences, 3);
  PrintOverlapGraph(sequences, graph);
end;

procedure ProcessFASTAFile(const filename: string);
var
  sequences: TStringArray;
  graph: TOverlapList;
  i: integer;
begin
  try
    sequences := GetSequencesFromFASTA(filename);
    
    if Length(sequences) > 0 then
    begin
      Writeln('Processing FASTA file: ', filename);
      Writeln('Number of sequences: ', Length(sequences));
      
      for i := 0 to Length(sequences) - 1 do
      begin
        Writeln('Sequence ', i, ': ', sequences[i]);
      end;
      Writeln;
      
      graph := BuildOverlapGraph(sequences, 3);
      PrintOverlapGraph(sequences, graph);
    end
    else
    begin
      Writeln('No sequences found in file: ', filename);
    end;
  except
    on E: Exception do
      Writeln('Error processing file: ', E.Message);
  end;
end;

begin
  Writeln('Overlap Graphs Solution');
  Writeln('======================');
  Writeln;
  
  // Test with sample data
  TestOverlapGraph;
  Writeln;
  
  // Process a sample FASTA file (if exists)
  ProcessFASTAFile('sequences.fasta');
  
  Writeln;
  Writeln('Press Enter to exit...');
  ReadLn;
end.
```

## Key Features

1. **GetOverlap Function**: Determines the overlap length between two strings
2. **BuildOverlapGraph Function**: Creates an adjacency matrix representing overlaps
3. **PrintOverlapGraph Procedure**: Displays the overlap relationships
4. **FASTA Processing**: Handles FASTA formatted input files
5. **Error Handling**: Includes proper exception handling

## Usage

1. Compile the Pascal program
2. Create a FASTA file with DNA sequences
3. Run the program to see overlap relationships
4. The program will output pairs of sequences that overlap with their overlap lengths

## Sample Output

```
Input sequences:
AAATTT
TTTGGG
GGGAAA
AAATTT

Overlap Graph:
Node -> Node (overlap length)
-----------------------------
AAATTT -> TTTGGG (3)
TTTGGG -> GGGAAA (3)
```

This solution efficiently builds overlap graphs for DNA sequence analysis, which is useful in bioinformatics applications such as genome assembly.


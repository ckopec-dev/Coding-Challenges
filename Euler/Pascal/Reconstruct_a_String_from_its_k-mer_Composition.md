# Euler Problem: Reconstruct a String from its k-mer Composition
## Pascal Implementation

```pascal
program EulerReconstructString;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Generics.Collections;

type
  TStringList = class(Classes.TStringList);
  TDictionary = class(Generics.Collections.TDictionary<string, Integer>);
  TGraph = class;

  // Node structure for De Bruijn graph
  TNode = record
    Name: string;
    InDegree: Integer;
    OutDegree: Integer;
    Edges: array of string;
  end;

  TGraph = class
  private
    FNodes: array of TNode;
    FNodeMap: TDictionary;
    FStartNode: string;
    FEndNode: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEdge(const From, To: string);
    procedure BuildFromKmers(const Kmers: array of string; K: Integer);
    function FindEulerianPath: string;
    function GetNodeIndex(const Name: string): Integer;
    procedure PrintGraph;
  end;

constructor TGraph.Create;
begin
  inherited Create;
  SetLength(FNodes, 0);
  FNodeMap := TDictionary.Create;
  FStartNode := '';
  FEndNode := '';
end;

destructor TGraph.Destroy;
begin
  FNodeMap.Free;
  inherited Destroy;
end;

function TGraph.GetNodeIndex(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FNodes) do
    if FNodes[i].Name = Name then
    begin
      Result := i;
      Exit;
    end;
end;

procedure TGraph.AddEdge(const From, To: string);
var
  FromIndex, ToIndex: Integer;
  i: Integer;
begin
  // Add source node if not exists
  FromIndex := GetNodeIndex(From);
  if FromIndex = -1 then
  begin
    SetLength(FNodes, Length(FNodes) + 1);
    FNodes[High(FNodes)].Name := From;
    FNodes[High(FNodes)].InDegree := 0;
    FNodes[High(FNodes)].OutDegree := 0;
    SetLength(FNodes[High(FNodes)].Edges, 0);
    FNodeMap.Add(From, High(FNodes));
    FromIndex := High(FNodes);
  end;

  // Add destination node if not exists
  ToIndex := GetNodeIndex(To);
  if ToIndex = -1 then
  begin
    SetLength(FNodes, Length(FNodes) + 1);
    FNodes[High(FNodes)].Name := To;
    FNodes[High(FNodes)].InDegree := 0;
    FNodes[High(FNodes)].OutDegree := 0;
    SetLength(FNodes[High(FNodes)].Edges, 0);
    FNodeMap.Add(To, High(FNodes));
    ToIndex := High(FNodes);
  end;

  // Add edge
  SetLength(FNodes[FromIndex].Edges, FNodes[FromIndex].OutDegree + 1);
  FNodes[FromIndex].Edges[FNodes[FromIndex].OutDegree] := To;
  FNodes[FromIndex].OutDegree := FNodes[FromIndex].OutDegree + 1;
  FNodes[ToIndex].InDegree := FNodes[ToIndex].InDegree + 1;
end;

procedure TGraph.BuildFromKmers(const Kmers: array of string; K: Integer);
var
  i: Integer;
  Prefix, Suffix: string;
begin
  for i := 0 to High(Kmers) do
  begin
    Prefix := Copy(Kmers[i], 1, K - 1);
    Suffix := Copy(Kmers[i], 2, K - 1);
    AddEdge(Prefix, Suffix);
  end;
end;

function TGraph.FindEulerianPath: string;
var
  Stack: array of string;
  ResultString: string;
  CurrentNode: string;
  CurrentIndex: Integer;
  NodeIndex: Integer;
begin
  // Find starting node (node with out-degree - in-degree = 1)
  // or any node with out-degree > 0
  FStartNode := '';
  for CurrentIndex := 0 to High(FNodes) do
  begin
    if FNodes[CurrentIndex].OutDegree > FNodes[CurrentIndex].InDegree then
    begin
      FStartNode := FNodes[CurrentIndex].Name;
      Break;
    end;
  end;

  if FStartNode = '' then
  begin
    // If no start node found, use first node
    if High(FNodes) >= 0 then
      FStartNode := FNodes[0].Name;
  end;

  SetLength(Stack, 0);
  ResultString := '';
  CurrentNode := FStartNode;

  while (Length(Stack) > 0) or (GetNodeIndex(CurrentNode) >= 0) do
  begin
    NodeIndex := GetNodeIndex(CurrentNode);
    if (NodeIndex >= 0) and (FNodes[NodeIndex].OutDegree > 0) then
    begin
      // Push current node to stack
      SetLength(Stack, Length(Stack) + 1);
      Stack[High(Stack)] := CurrentNode;
      
      // Move to next node
      CurrentNode := FNodes[NodeIndex].Edges[0];
      
      // Remove edge from graph
      for i := 0 to FNodes[NodeIndex].OutDegree - 2 do
        FNodes[NodeIndex].Edges[i] := FNodes[NodeIndex].Edges[i + 1];
      FNodes[NodeIndex].OutDegree := FNodes[NodeIndex].OutDegree - 1;
    end
    else
    begin
      ResultString := CurrentNode + ResultString;
      if Length(Stack) > 0 then
      begin
        CurrentNode := Stack[High(Stack)];
        SetLength(Stack, Length(Stack) - 1);
      end
      else
        Break;
    end;
  end;

  Result := ResultString;
end;

procedure TGraph.PrintGraph;
var
  i, j: Integer;
begin
  Writeln('De Bruijn Graph:');
  for i := 0 to High(FNodes) do
  begin
    Write(FNodes[i].Name, ' -> ');
    for j := 0 to FNodes[i].OutDegree - 1 do
    begin
      Write(FNodes[i].Edges[j]);
      if j < FNodes[i].OutDegree - 1 then
        Write(', ');
    end;
    Writeln;
  end;
end;

// Main program
var
  Kmers: array of string;
  K: Integer;
  Graph: TGraph;
  ResultString: string;
  i: Integer;

begin
  try
    // Example input - k-mers from Euler problem
    K := 4;
    SetLength(Kmers, 5);
    Kmers[0] := 'CTTA';
    Kmers[1] := 'GCCT';
    Kmers[2] := 'TACC';
    Kmers[3] := 'GGCT';
    Kmers[4] := 'GCTT';
    
    Writeln('Input k-mers:');
    for i := 0 to High(Kmers) do
      Writeln(Kmers[i]);
    Writeln;
    
    // Build De Bruijn graph
    Graph := TGraph.Create;
    try
      Graph.BuildFromKmers(Kmers, K);
      Graph.PrintGraph;
      Writeln;
      
      // Find Eulerian path
      ResultString := Graph.FindEulerianPath;
      
      Writeln('Reconstructed string:');
      Writeln(ResultString);
      
    finally
      Graph.Free;
    end;
    
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  
  Readln;
end.
```

## Problem Explanation

This Pascal program solves the Euler problem of reconstructing a string from its k-mer composition. The approach uses:

1. **De Bruijn Graph Construction**: Each k-mer is represented as an edge between (k-1)-mers
2. **Eulerian Path Finding**: Uses Hierholzer's algorithm to find a path that visits every edge exactly once
3. **String Reconstruction**: The path through the graph reconstructs the original string

## Key Components

### TGraph Class
- **AddEdge**: Creates nodes and edges for the De Bruijn graph
- **BuildFromKmers**: Constructs the graph from input k-mers
- **FindEulerianPath**: Implements Eulerian path finding using stack-based approach
- **PrintGraph**: Visualizes the constructed graph

### Algorithm Steps
1. Convert k-mers to De Bruijn graph edges
2. Find Eulerian path through the graph
3. Reconstruct original string from path

### Time Complexity
- Graph construction: O(n × k) where n is number of k-mers
- Eulerian path finding: O(n)
- Overall: O(n × k)

### Space Complexity
- O(n × k) for storing the graph

This implementation handles the specific case of reconstructing a string from k-mer composition, which is a fundamental problem in bioinformatics and sequence assembly.


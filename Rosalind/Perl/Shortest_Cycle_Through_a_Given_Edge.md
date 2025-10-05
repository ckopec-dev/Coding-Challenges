# Rosalind Problem: Shortest Cycle Through a Given Edge

## Problem Understanding

We need to find the shortest cycle that passes through a specific given edge in an undirected graph.

## Approach

1. **Graph Representation**: Use adjacency list representation
2. **Algorithm**: 
   - Remove the given edge temporarily
   - Find the shortest path between the two vertices of the removed edge using BFS
   - Add the edge back and return the path length + 1 (for the original edge)

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Read input data
my @lines = <STDIN>;
chomp @lines;

# Parse the graph
my %graph;
my ($n, $m) = split /\s+/, $lines[0];
my @edges;

for my $i (1..$m) {
    my ($u, $v) = split /\s+/, $lines[$i];
    push @edges, [$u, $v];
    
    # Build adjacency list
    push @{$graph{$u}}, $v;
    push @{$graph{$v}}, $u;
}

# Parse the given edge (1-indexed)
my ($start_edge, $end_edge) = split /\s+/, $lines[$m + 1];

# Function to find shortest path using BFS
sub bfs_shortest_path {
    my ($graph, $start, $end) = @_;
    
    my %visited;
    my @queue = ($start);
    $visited{$start} = 0;
    
    while (@queue) {
        my $current = shift @queue;
        
        if ($current eq $end) {
            return $visited{$current};
        }
        
        for my $neighbor (@{$graph->{$current}}) {
            if (!exists $visited{$neighbor}) {
                $visited{$neighbor} = $visited{$current} + 1;
                push @queue, $neighbor;
            }
        }
    }
    
    return -1; # No path found
}

# Find shortest cycle through the given edge
my $shortest_path_length = bfs_shortest_path(\%graph, $start_edge, $end_edge);

if ($shortest_path_length == -1) {
    print "No cycle exists\n";
} else {
    # Add 1 for the original edge that connects start_edge and end_edge
    my $cycle_length = $shortest_path_length + 1;
    print "$cycle_length\n";
}
```

## Alternative Solution (More Robust)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read all input lines
my @input_lines = <STDIN>;
chomp @input_lines;

# Parse graph size
my ($n, $m) = split /\s+/, $input_lines[0];

# Parse edges and build adjacency list
my %adj_list;
my @edges;

for my $i (1..$m) {
    my ($u, $v) = split /\s+/, $input_lines[$i];
    push @edges, [$u, $v];
    
    # Build adjacency list (undirected graph)
    push @{$adj_list{$u}}, $v;
    push @{$adj_list{$v}}, $u;
}

# Parse the specific edge to find cycle through
my ($start_node, $end_node) = split /\s+/, $input_lines[$m + 1];

# BFS to find shortest path between start_node and end_node
sub find_shortest_path {
    my ($graph, $start, $end) = @_;
    
    my %visited;
    my @queue = ($start);
    $visited{$start} = 0;
    
    while (@queue) {
        my $current = shift @queue;
        
        if ($current eq $end) {
            return $visited{$current};
        }
        
        for my $neighbor (@{$graph->{$current}}) {
            if (!exists $visited{$neighbor}) {
                $visited{$neighbor} = $visited{$current} + 1;
                push @queue, $neighbor;
            }
        }
    }
    
    return -1; # No path found
}

# Find shortest cycle through the given edge
my $path_length = find_shortest_path(\%adj_list, $start_node, $end_node);

if ($path_length == -1) {
    print "No cycle possible\n";
} else {
    # Cycle length = shortest path + 1 (the edge itself)
    my $cycle_length = $path_length + 1;
    print "$cycle_length\n";
}
```

## Input Format

The input should be formatted as:
```
n m
u1 v1
u2 v2
...
um vm
start_node end_node
```

Where:
- `n` is the number of vertices
- `m` is the number of edges
- Each line after the first contains an edge (u, v)
- The last line contains the specific edge (start_node, end_node) to find cycle through

## Example

Input:
```
4 5
1 2
2 3
3 4
4 1
1 3
2 3
```

Output:
```
3
```

This represents a triangle (cycle of length 3) passing through edge (2,3).


# Rosalind Problem: Connected Components - Perl Solution

## Problem Understanding

The connected components problem asks us to find how many separate groups (connected components) exist in an undirected graph.

## Approach

I'll use a **Depth-First Search (DFS)** approach to traverse the graph and count connected components:
1. Keep track of visited nodes
2. For each unvisited node, start a new DFS traversal
3. Count each such traversal as one connected component

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the graph
my %graph = ();
my $num_nodes = 0;
my $num_edges = 0;

# Process each line to build adjacency list
foreach my $line (@lines) {
    if ($line =~ /^(\d+)\s+(\d+)$/) {
        # First line contains number of nodes and edges
        $num_nodes = $1;
        $num_edges = $2;
    } elsif ($line =~ /^(\d+)\s+(\d+)$/) {
        # Subsequent lines contain edges
        my ($node1, $node2) = ($1, $2);
        
        # Add to adjacency list (undirected graph)
        push @{$graph{$node1}}, $node2;
        push @{$graph{$node2}}, $node1;
    }
}

# Track visited nodes
my %visited = ();

# Count connected components
my $components = 0;

# For each node, if not visited, start new component
foreach my $node (sort keys %graph) {
    if (!exists $visited{$node}) {
        # Start DFS from this node
        dfs($node, \%graph, \%visited);
        $components++;
    }
}

# Handle isolated nodes (nodes with no edges)
my $isolated_nodes = 0;
foreach my $i (1..$num_nodes) {
    if (!exists $graph{$i}) {
        $isolated_nodes++;
    }
}

# Add isolated nodes to component count
$components += $isolated_nodes;

print "$components\n";

# Depth-First Search function
sub dfs {
    my ($node, $graph_ref, $visited_ref) = @_;
    
    # Mark current node as visited
    $visited_ref->{$node} = 1;
    
    # Visit all neighbors
    foreach my $neighbor (@{$graph_ref->{$node}}) {
        if (!exists $visited_ref->{$neighbor}) {
            dfs($neighbor, $graph_ref, $visited_ref);
        }
    }
}
```

## Alternative Cleaner Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read all input lines
my @input = <STDIN>;
chomp @input;

# Build adjacency list representation
my %adj_list = ();

# Process edges (skip first line if it contains node/edge counts)
foreach my $line (@input) {
    next if $line =~ /^\d+\s+\d+$/;  # Skip header line
    
    if ($line =~ /^(\d+)\s+(\d+)$/) {
        my ($u, $v) = ($1, $2);
        push @{$adj_list{$u}}, $v;
        push @{$adj_list{$v}}, $u;
    }
}

# Track visited nodes
my %visited = ();

# Count connected components using DFS
my $count = 0;

# For each node in the graph
foreach my $node (keys %adj_list) {
    if (!exists $visited{$node}) {
        # Start new component
        dfs($node, \%adj_list, \%visited);
        $count++;
    }
}

# Count isolated nodes (nodes not appearing in any edge)
my %all_nodes = ();
foreach my $line (@input) {
    if ($line =~ /^(\d+)\s+(\d+)$/) {
        $all_nodes{$1} = 1;
        $all_nodes{$2} = 1;
    }
}

# Add isolated nodes to count
my $total_nodes = keys %all_nodes;
my $visited_count = keys %visited;

# Handle case where some nodes might be isolated
if ($total_nodes > $visited_count) {
    $count += ($total_nodes - $visited_count);
}

print "$count\n";

sub dfs {
    my ($node, $graph_ref, $visited_ref) = @_;
    
    $visited_ref->{$node} = 1;
    
    foreach my $neighbor (@{$graph_ref->{$node}}) {
        if (!exists $visited_ref->{$neighbor}) {
            dfs($neighbor, $graph_ref, $visited_ref);
        }
    }
}
```

## Sample Input/Output

**Input:**
```
5 4
1 2
2 3
3 4
4 5
```

**Output:**
```
1
```

**Input:**
```
5 3
1 2
3 4
4 5
```

**Output:**
```
3
```

## Explanation

1. **Graph Representation**: Uses an adjacency list to represent the undirected graph
2. **DFS Traversal**: For each unvisited node, performs DFS to mark all reachable nodes as visited
3. **Component Counting**: Each DFS call corresponds to one connected component
4. **Edge Cases**: Handles isolated nodes and ensures all nodes are counted

The time complexity is O(V + E) where V is the number of vertices and E is the number of edges, and space complexity is O(V + E) for storing the adjacency list and visited set.


# Rosalind Problem: Building a Heap (BAW2)

## Problem Description

Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: An array built from A that satisfies the heap property.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $n = <STDIN>;
chomp $n;
my $array_line = <STDIN>;
chomp $array_line;

# Parse the array
my @A = split /\s+/, $array_line;

# Convert to 0-indexed (Rosalind uses 1-indexed arrays)
for my $i (0..$#A) {
    $A[$i] = $A[$i] - 1;  # Adjust for 0-based indexing
}

# Build heap function
sub build_heap {
    my @arr = @_;
    my $size = scalar @arr;
    
    # Start from the last non-leaf node and heapify each node
    for my $i (reverse 0..int($size/2) - 1) {
        heapify(\@arr, $size, $i);
    }
    
    return @arr;
}

# Heapify function to maintain max heap property
sub heapify {
    my ($arr, $size, $i) = @_;
    
    my $largest = $i;
    my $left = 2 * $i + 1;
    my $right = 2 * $i + 2;
    
    # If left child exists and is greater than root
    if ($left < $size && $arr->[$left] > $arr->[$largest]) {
        $largest = $left;
    }
    
    # If right child exists and is greater than largest so far
    if ($right < $size && $arr->[$right] > $arr->[$largest]) {
        $largest = $right;
    }
    
    # If largest is not root
    if ($largest != $i) {
        # Swap
        my $temp = $arr->[$i];
        $arr->[$i] = $arr->[$largest];
        $arr->[$largest] = $temp;
        
        # Recursively heapify the affected sub-tree
        heapify($arr, $size, $largest);
    }
}

# Build the heap
my @heap = build_heap(@A);

# Convert back to 1-indexed for output
for my $i (0..$#heap) {
    $heap[$i] += 1;
}

# Print result
print join(" ", @heap) . "\n";
```

## Alternative Cleaner Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $n = <STDIN>;
chomp $n;
my $input = <STDIN>;
chomp $input;

# Parse array
my @A = split /\s+/, $input;

# Convert to 0-indexed (as Perl arrays are 0-indexed)
# But we'll work with the array directly

# Build max heap using bottom-up approach
sub build_max_heap {
    my @arr = @_;
    my $n = scalar @arr;
    
    # Start from last non-leaf node and heapify all nodes
    for my $i (reverse 0..int($n/2) - 1) {
        heapify(\@arr, $n, $i);
    }
    
    return @arr;
}

sub heapify {
    my ($arr, $n, $i) = @_;
    
    my $largest = $i;
    my $left = 2 * $i + 1;
    my $right = 2 * $i + 2;
    
    # Find largest among root, left child and right child
    if ($left < $n && $arr->[$left] > $arr->[$largest]) {
        $largest = $left;
    }
    
    if ($right < $n && $arr->[$right] > $arr->[$largest]) {
        $largest = $right;
    }
    
    # If largest is not root
    if ($largest != $i) {
        # Swap
        ($arr->[$i], $arr->[$largest]) = ($arr->[$largest], $arr->[$i]);
        
        # Recursively heapify the affected sub-tree
        heapify($arr, $n, $largest);
    }
}

# Build heap and output result
my @result = build_max_heap(@A);

print join(" ", @result) . "\n";
```

## How It Works

1. **Input parsing**: Read the array size and the array elements
2. **Heap construction**: Use the bottom-up approach to build a max heap:
   - Start from the last non-leaf node (at index n/2 - 1)
   - Heapify each node moving upward to the root
3. **Heapify operation**: 
   - Compare parent with its children
   - If any child is larger, swap and continue recursively
4. **Output**: Print the resulting heap array

## Time Complexity
- **Time**: O(n) for building the heap
- **Space**: O(log n) for recursion stack

## Example Input/Output

**Input:**
```
5
1 3 5 7 2
```

**Output:**
```
7 3 5 1 2
```

The output represents a valid max heap where each parent node is greater than or equal to its children.


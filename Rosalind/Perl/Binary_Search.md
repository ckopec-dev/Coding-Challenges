# Rosalind Problem: Binary Search - Perl Solution

## Problem Understanding

The binary search problem requires finding the position of a target value in a sorted array using binary search algorithm.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the input
my @array = split /\s+/, $lines[1];
my @targets = split /\s+/, $lines[3];

# Binary search function
sub binary_search {
    my ($array_ref, $target) = @_;
    my $left = 0;
    my $right = @$array_ref - 1;
    
    while ($left <= $right) {
        my $mid = int(($left + $right) / 2);
        
        if ($array_ref->[$mid] == $target) {
            return $mid + 1; # 1-based indexing
        } elsif ($array_ref->[$mid] < $target) {
            $left = $mid + 1;
        } else {
            $right = $mid - 1;
        }
    }
    
    return -1; # Not found
}

# Process each target
my @results = ();
foreach my $target (@targets) {
    my $position = binary_search(\@array, $target);
    push @results, $position;
}

# Output results
print join(" ", @results) . "\n";
```

## How it works

1. **Input parsing**: Read the input lines and extract the sorted array and target values
2. **Binary search function**: 
   - Uses two pointers (`left` and `right`) to define the search range
   - Calculates the middle index and compares the middle element with the target
   - Adjusts search range based on comparison
   - Returns 1-based index if found, -1 if not found
3. **Processing**: Apply binary search to each target value
4. **Output**: Print all results separated by spaces

## Example

**Input:**
```
5
1 5 8 12 13
5
6 1 13 8 0
```

**Output:**
```
-1 1 5 3 -1
```

## Time Complexity
- **Time**: O(log n) for each search
- **Space**: O(1) additional space

## Key Features
- Handles 1-based indexing as required by Rosalind
- Returns -1 for elements not found
- Efficient binary search implementation
- Proper input/output handling for Rosalind format


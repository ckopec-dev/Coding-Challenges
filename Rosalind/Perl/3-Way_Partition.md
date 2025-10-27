# Rosalind Problem: 3-Way Partition

## Problem Description
Given an array of integers and a pivot value, partition the array into three parts:
- Elements less than the pivot
- Elements equal to the pivot  
- Elements greater than the pivot

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = <STDIN>;
chomp $input;

# Parse the input
my @array = split /\s+/, $input;
my $pivot = $array[-1];  # Last element is the pivot
pop @array;              # Remove pivot from array

# 3-way partition
my @less = ();      # Elements less than pivot
my @equal = ();     # Elements equal to pivot
my @greater = ();   # Elements greater than pivot

foreach my $element (@array) {
    if ($element < $pivot) {
        push @less, $element;
    } elsif ($element == $pivot) {
        push @equal, $element;
    } else {
        push @greater, $element;
    }
}

# Output the result
print join(" ", @less, @equal, @greater) . "\n";
```

## Alternative Implementation (In-place)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = <STDIN>;
chomp $input;

# Parse the input
my @array = split /\s+/, $input;
my $pivot = $array[-1];  # Last element is the pivot
pop @array;              # Remove pivot from array

# 3-way partition using indices
my $low = 0;
my $mid = 0;
my $high = $#array;

while ($mid <= $high) {
    if ($array[$mid] < $pivot) {
        # Swap with low boundary
        ($array[$low], $array[$mid]) = ($array[$mid], $array[$low]);
        $low++;
        $mid++;
    } elsif ($array[$mid] > $pivot) {
        # Swap with high boundary
        ($array[$mid], $array[$high]) = ($array[$high], $array[$mid]);
        $high--;
    } else {
        # Element equals pivot
        $mid++;
    }
}

# Output the result
print join(" ", @array) . "\n";
```

## Sample Input/Output

**Input:**
```
9 12 3 5 14 10 10 10 10 10
```

**Output:**
```
9 3 5 10 10 10 10 10 12 14
```

## Explanation

The 3-way partition algorithm works by:
1. **First approach**: Creates three separate arrays and distributes elements based on comparison with pivot
2. **Second approach**: Uses three pointers (low, mid, high) to partition the array in-place
3. Elements less than pivot go to the left
4. Elements equal to pivot stay in the middle  
5. Elements greater than pivot go to the right

The time complexity is O(n) and space complexity is O(1) for the in-place version.


# Finding the Longest Multiple Repeat - Rosalind Problem Solution in Perl

## Problem Understanding

The task is to find the longest substring that appears at least k times in a given DNA sequence.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = do { local $/; <STDIN> };
my @lines = split /\n/, $input;
my $sequence = $lines[0];

# Read k value (number of repetitions)
my $k = $lines[1];

# Function to find longest multiple repeat
sub find_longest_multiple_repeat {
    my ($seq, $k) = @_;
    
    my $max_length = 0;
    my $result = "";
    
    # Try all possible substring lengths from longest to shortest
    for my $len (reverse 1..length($seq)) {
        # Try all possible starting positions for current length
        for my $start (0..length($seq) - $len) {
            my $substring = substr($seq, $start, $len);
            
            # Count occurrences of this substring
            my $count = 0;
            my $pos = 0;
            while (($pos = index($seq, $substring, $pos)) != -1) {
                $count++;
                $pos++;
            }
            
            # If this substring appears at least k times and is longer than current max
            if ($count >= $k && $len > $max_length) {
                $max_length = $len;
                $result = $substring;
            }
        }
        
        # Early termination: if we found a repeat of current length, no need to check shorter ones
        if ($max_length > 0) {
            last;
        }
    }
    
    return $result;
}

# Find and print the result
my $longest_repeat = find_longest_multiple_repeat($sequence, $k);
print $longest_repeat . "\n";
```

## Alternative Optimized Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $input = do { local $/; <STDIN> };
my @lines = split /\n/, $input;
my $sequence = $lines[0];
my $k = $lines[1];

# Optimized approach using hash to count substrings
sub find_longest_multiple_repeat_optimized {
    my ($seq, $k) = @_;
    
    my %substring_count;
    my $max_length = 0;
    my $result = "";
    
    # Generate all substrings and count them
    for my $len (1..length($seq)) {
        for my $start (0..length($seq) - $len) {
            my $substring = substr($seq, $start, $len);
            $substring_count{$substring}++;
        }
    }
    
    # Find the longest substring that appears at least k times
    for my $substring (keys %substring_count) {
        if ($substring_count{$substring} >= $k && length($substring) > $max_length) {
            $max_length = length($substring);
            $result = $substring;
        }
    }
    
    return $result;
}

# For large inputs, use binary search approach
sub find_longest_multiple_repeat_binary {
    my ($seq, $k) = @_;
    
    my $low = 1;
    my $high = length($seq);
    my $result = "";
    
    while ($low <= $high) {
        my $mid = int(($low + $high) / 2);
        my $found = 0;
        
        # Check if there exists a repeat of length mid
        my %seen;
        for my $start (0..length($seq) - $mid) {
            my $substring = substr($seq, $start, $mid);
            $seen{$substring}++;
            if ($seen{$substring} >= $k) {
                $found = 1;
                $result = $substring;
                last;
            }
        }
        
        if ($found) {
            $low = $mid + 1;
        } else {
            $high = $mid - 1;
        }
    }
    
    return $result;
}

# Main execution
my $longest_repeat = find_longest_multiple_repeat_binary($sequence, $k);
print $longest_repeat . "\n";
```

## Example Usage

For input:
```
ACGTACGTACGT
3
```

Expected output:
```
ACGT
```

## Key Points

1. **Input Format**: First line contains DNA sequence, second line contains k value
2. **Approach**: The solution uses either a brute force method or binary search for efficiency
3. **Time Complexity**: O(n³) for brute force, O(n² log n) for binary search approach
4. **Space Complexity**: O(n²) for storing substring counts

## Test Case

```perl
# Test with sample data
my $test_seq = "ACGTACGTACGT";
my $test_k = 3;
my $result = find_longest_multiple_repeat($test_seq, $test_k);
print "Result: $result\n";  # Should output "ACGT"
```

This solution handles the core requirements of finding the longest substring that appears at least k times in the given DNA sequence.


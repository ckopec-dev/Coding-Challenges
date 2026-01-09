# Rosalind Problem: DistanceBetweenPatternAndStrings

## Problem Description
Given a DNA pattern and a collection of DNA strings, find the minimum Hamming distance between the pattern and all possible k-mers in the strings.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to calculate Hamming distance between two strings
sub hamming_distance {
    my ($str1, $str2) = @_;
    my $distance = 0;
    
    for my $i (0..length($str1)-1) {
        if (substr($str1, $i, 1) ne substr($str2, $i, 1)) {
            $distance++;
        }
    }
    
    return $distance;
}

# Function to get all k-mers from a string
sub get_kmers {
    my ($string, $k) = @_;
    my @kmers = ();
    
    for my $i (0..length($string)-$k) {
        push @kmers, substr($string, $i, $k);
    }
    
    return @kmers;
}

# Function to find minimum Hamming distance between pattern and strings
sub distance_between_pattern_and_strings {
    my ($pattern, @strings) = @_;
    my $k = length($pattern);
    my $total_distance = 0;
    
    # For each string, find the minimum distance to any k-mer in that string
    for my $string (@strings) {
        my @kmers = get_kmers($string, $k);
        my $min_distance = length($pattern) + 1;  # Initialize with large value
        
        for my $kmer (@kmers) {
            my $distance = hamming_distance($pattern, $kmer);
            if ($distance < $min_distance) {
                $min_distance = $distance;
            }
        }
        
        $total_distance += $min_distance;
    }
    
    return $total_distance;
}

# Read input from stdin
my $pattern = <STDIN>;
chomp $pattern;

my @strings = ();
while (my $line = <STDIN>) {
    chomp $line;
    push @strings, $line;
}

# Calculate and print the result
my $result = distance_between_pattern_and_strings($pattern, @strings);
print "$result\n";
```

## Example Usage

**Input:**
```
AAA
TTACCTTAAC
GATATCTGAT
ACGGCGTTCG
CCCTAAAGAG
CGTCAGAGGT
```

**Output:**
```
5
```

## Explanation

1. **Hamming Distance Function**: Calculates the number of positions where two strings differ
2. **K-mers Function**: Extracts all possible k-length substrings from a given string
3. **Main Algorithm**: 
   - For each DNA string, extract all k-mers (where k is pattern length)
   - Find the minimum Hamming distance between the pattern and any k-mer
   - Sum up these minimum distances across all strings

The algorithm finds the minimum distance between the given pattern and all possible k-mers in the collection of DNA strings, which represents the total distance between the pattern and the strings.


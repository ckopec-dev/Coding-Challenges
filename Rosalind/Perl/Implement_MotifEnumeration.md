# Rosalind Problem: Implement MotifEnumeration in Perl

## Problem Description
Given a collection of strings, find all k-mers that appear in every string with at most d mismatches.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to generate all k-mers of length k from a string
sub get_kmers {
    my ($text, $k) = @_;
    my @kmers = ();
    
    for my $i (0..length($text) - $k) {
        push @kmers, substr($text, $i, $k);
    }
    
    return @kmers;
}

# Function to calculate Hamming distance between two strings
sub hamming_distance {
    my ($str1, $str2) = @_;
    my $distance = 0;
    
    for my $i (0..length($str1) - 1) {
        if (substr($str1, $i, 1) ne substr($str2, $i, 1)) {
            $distance++;
        }
    }
    
    return $distance;
}

# Function to generate all possible k-mers of length k
sub get_all_kmers {
    my ($k) = @_;
    my @kmers = ('A', 'C', 'G', 'T');
    
    for my $i (2..$k) {
        my @new_kmers = ();
        for my $kmer (@kmers) {
            push @new_kmers, $kmer . 'A';
            push @new_kmers, $kmer . 'C';
            push @new_kmers, $kmer . 'G';
            push @new_kmers, $kmer . 'T';
        }
        @kmers = @new_kmers;
    }
    
    return @kmers;
}

# Function to check if a k-mer appears in a string with at most d mismatches
sub appears_in_string {
    my ($kmer, $string, $d) = @_;
    
    for my $i (0..length($string) - length($kmer)) {
        my $substring = substr($string, $i, length($kmer));
        if (hamming_distance($kmer, $substring) <= $d) {
            return 1;
        }
    }
    
    return 0;
}

# Main function to solve motif enumeration
sub motif_enumeration {
    my ($d, $k, @dna) = @_;
    my @candidates = get_all_kmers($k);
    my @motifs = ();
    
    for my $kmer (@candidates) {
        my $found_in_all = 1;
        
        for my $string (@dna) {
            if (!appears_in_string($kmer, $string, $d)) {
                $found_in_all = 0;
                last;
            }
        }
        
        if ($found_in_all) {
            push @motifs, $kmer;
        }
    }
    
    return @motifs;
}

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse input
my ($k, $d) = split /\s+/, $lines[0];
my @dna = @lines[1..$#lines];

# Solve motif enumeration
my @motifs = motif_enumeration($d, $k, @dna);

# Print results
print join(" ", @motifs) . "\n";
```

## Example Usage

**Input:**
```
3 1
ATTTGGC TGCCTTA CGGTATC GAAAATT
```

**Output:**
```
ATA ATT GTT TTT
```

## Explanation

1. **Input parsing**: Read k (k-mer length) and d (maximum mismatches) from the first line, then read DNA sequences.

2. **Generate candidates**: Create all possible k-mers of length k using the four nucleotides (A, C, G, T).

3. **Check each candidate**: For each k-mer, verify if it appears in every DNA string with at most d mismatches.

4. **Hamming distance**: Calculate the number of positions where two strings differ.

5. **Output**: Print all k-mers that satisfy the motif enumeration criteria.

## Time Complexity
- O(4^k × n × k) where n is the total length of all DNA strings
- This is because we generate 4^k candidates and for each, we check all positions in all strings

## Space Complexity
- O(4^k) for storing the candidate k-mers

This solution efficiently finds all motifs that appear in every DNA string with at most d mismatches, solving the motif enumeration problem as required by Rosalind.


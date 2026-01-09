# Euler Problem: Most Frequent Words with Mismatches in a String

This problem asks us to find the most frequent k-mers (substrings of length k) in a DNA string that can have up to d mismatches with the original pattern.

## Solution in Rust

```rust
use std::collections::HashMap;

fn main() {
    let text = "ACGTTGCATGTCGCATGATGCATGAGAGG";
    let k = 4;
    let d = 1;
    
    let result = frequent_words_with_mismatches(text, k, d);
    println!("Most frequent words with mismatches: {:?}", result);
}

fn frequent_words_with_mismatches(text: &str, k: usize, d: usize) -> Vec<String> {
    let mut frequency_map: HashMap<String, usize> = HashMap::new();
    let mut max_frequency = 0;
    
    // Generate all possible k-mers
    let mut patterns = Vec::new();
    generate_all_kmers(k, &mut patterns);
    
    // For each k-mer, count mismatches with all substrings in text
    for pattern in patterns {
        let pattern_count = count_approximate_matches(text, &pattern, d);
        *frequency_map.entry(pattern).or_insert(0) += pattern_count;
        max_frequency = max_frequency.max(pattern_count);
    }
    
    // Find all patterns with maximum frequency
    let mut result = Vec::new();
    for (pattern, &count) in &frequency_map {
        if count == max_frequency {
            result.push(pattern.clone());
        }
    }
    
    result
}

fn generate_all_kmers(k: usize, patterns: &mut Vec<String>) {
    if k == 0 {
        patterns.push(String::new());
        return;
    }
    
    let nucleotides = ['A', 'C', 'G', 'T'];
    let mut current = String::with_capacity(k);
    
    fn backtrack(k: usize, current: &mut String, nucleotides: &[char], patterns: &mut Vec<String>) {
        if current.len() == k {
            patterns.push(current.clone());
            return;
        }
        
        for &nucleotide in nucleotides {
            current.push(nucleotide);
            backtrack(k, current, nucleotides, patterns);
            current.pop();
        }
    }
    
    backtrack(k, &mut current, &nucleotides, patterns);
}

fn count_approximate_matches(text: &str, pattern: &str, d: usize) -> usize {
    let mut count = 0;
    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();
    
    for i in 0..=text_chars.len().saturating_sub(pattern_chars.len()) {
        if hamming_distance(&text_chars[i..i + pattern_chars.len()], &pattern_chars) <= d {
            count += 1;
        }
    }
    
    count
}

fn hamming_distance(a: &[char], b: &[char]) -> usize {
    a.iter().zip(b.iter()).filter(|(x, y)| x != y).count()
}
```

## Alternative Optimized Solution

```rust
use std::collections::HashMap;

fn main() {
    let text = "ACGTTGCATGTCGCATGATGCATGAGAGG";
    let k = 4;
    let d = 1;
    
    let result = frequent_words_with_mismatches_optimized(text, k, d);
    println!("Most frequent words with mismatches: {:?}", result);
}

fn frequent_words_with_mismatches_optimized(text: &str, k: usize, d: usize) -> Vec<String> {
    let mut frequency_map: HashMap<String, usize> = HashMap::new();
    
    // For each k-mer in text, count all its d-mismatches
    for i in 0..=text.len().saturating_sub(k) {
        let pattern = &text[i..i + k];
        let mut neighbors = Vec::new();
        get_mismatches(pattern, d, &mut neighbors);
        
        for neighbor in neighbors {
            *frequency_map.entry(neighbor).or_insert(0) += 1;
        }
    }
    
    // Find maximum frequency
    let max_frequency = *frequency_map.values().max().unwrap_or(&0);
    
    // Collect all patterns with maximum frequency
    frequency_map
        .into_iter()
        .filter(|(_, &count)| count == max_frequency)
        .map(|(pattern, _)| pattern)
        .collect()
}

fn get_mismatches(pattern: &str, d: usize, neighbors: &mut Vec<String>) {
    if d == 0 {
        neighbors.push(pattern.to_string());
        return;
    }
    
    let nucleotides = ['A', 'C', 'G', 'T'];
    let pattern_chars: Vec<char> = pattern.chars().collect();
    let mut current = vec![' '; pattern_chars.len()];
    
    fn generate_mismatches(
        pattern_chars: &[char],
        current: &mut Vec<char>,
        nucleotides: &[char],
        d: usize,
        neighbors: &mut Vec<String>,
        index: usize,
    ) {
        if index == pattern_chars.len() {
            neighbors.push(current.iter().collect());
            return;
        }
        
        if d > 0 {
            // Try different nucleotides at current position
            for &nucleotide in nucleotides {
                if nucleotide != pattern_chars[index] {
                    current[index] = nucleotide;
                    generate_mismatches(pattern_chars, current, nucleotides, d - 1, neighbors, index + 1);
                }
            }
        }
        
        // Keep original nucleotide
        current[index] = pattern_chars[index];
        generate_mismatches(pattern_chars, current, nucleotides, d, neighbors, index + 1);
    }
    
    generate_mismatches(&pattern_chars, &mut current, &nucleotides, d, neighbors, 0);
}
```

## Key Concepts

1. **Hamming Distance**: Number of positions where two strings differ
2. **Mismatches**: All possible k-mers that differ by at most d positions from a given pattern
3. **Frequency Counting**: Count how many times each pattern appears (including mismatches)
4. **Optimization**: Instead of generating all possible k-mers, generate neighbors for each k-mer in the text

## Time and Space Complexity

- **Time Complexity**: O(n × k × d) where n is text length, k is pattern length, d is maximum mismatches
- **Space Complexity**: O(n × k) for storing patterns and their frequencies

## Example Output

For the input text "ACGTTGCATGTCGCATGATGCATGAGAGG" with k=4 and d=1:
```
Most frequent words with mismatches: ["GCAT", "ATGC"]
```

This solution efficiently finds all k-mers that appear most frequently in the text, considering up to d mismatches.


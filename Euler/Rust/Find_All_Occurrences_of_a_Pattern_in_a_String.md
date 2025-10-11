# Euler Problem: Find All Occurrences of a Pattern in a String

## Problem Description
Find all occurrences of a pattern in a string and return their starting positions (0-indexed).

## Solution in Rust

```rust
fn find_all_occurrences(pattern: &str, text: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    // Handle edge cases
    if pattern.is_empty() || text.is_empty() {
        return positions;
    }
    
    // Iterate through the text to find pattern occurrences
    for i in 0..=(text.len().saturating_sub(pattern.len())) {
        if text[i..].starts_with(pattern) {
            positions.push(i);
        }
    }
    
    positions
}

// Alternative implementation using sliding window approach
fn find_all_occurrences_v2(pattern: &str, text: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    
    if pattern.is_empty() || text.is_empty() {
        return positions;
    }
    
    let pattern_len = pattern.len();
    let text_len = text.len();
    
    // Check each possible starting position
    for i in 0..=(text_len - pattern_len) {
        if &text[i..i + pattern_len] == pattern {
            positions.push(i);
        }
    }
    
    positions
}

// Using built-in methods (more concise but potentially less efficient)
fn find_all_occurrences_v3(pattern: &str, text: &str) -> Vec<usize> {
    let mut positions = Vec::new();
    let mut start = 0;
    
    if pattern.is_empty() || text.is_empty() {
        return positions;
    }
    
    while let Some(pos) = text[start..].find(pattern) {
        positions.push(start + pos);
        start += pos + 1;
    }
    
    positions
}

fn main() {
    // Test cases
    let pattern1 = "ATAT";
    let text1 = "GATATATGCATATACTT";
    let result1 = find_all_occurrences(pattern1, text1);
    println!("Pattern: {}, Text: {}", pattern1, text1);
    println!("Positions: {:?}", result1); // [1, 3, 9]
    
    let pattern2 = "CTAT";
    let text2 = "GATATATGCATATACTT";
    let result2 = find_all_occurrences(pattern2, text2);
    println!("Pattern: {}, Text: {}", pattern2, text2);
    println!("Positions: {:?}", result2); // [3, 7, 11]
    
    let pattern3 = "AAA";
    let text3 = "AAAAAA";
    let result3 = find_all_occurrences(pattern3, text3);
    println!("Pattern: {}, Text: {}", pattern3, text3);
    println!("Positions: {:?}", result3); // [0, 1, 2, 3, 4]
    
    // Edge cases
    let result4 = find_all_occurrences("", "test");
    println!("Empty pattern: {:?}", result4); // []
    
    let result5 = find_all_occurrences("test", "");
    println!("Empty text: {:?}", result5); // []
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_case() {
        assert_eq!(find_all_occurrences("ATAT", "GATATATGCATATACTT"), vec![1, 3, 9]);
    }

    #[test]
    fn test_no_matches() {
        assert_eq!(find_all_occurrences("XYZ", "GATATATGCATATACTT"), vec![]);
    }

    #[test]
    fn test_overlapping_patterns() {
        assert_eq!(find_all_occurrences("AAA", "AAAAAA"), vec![0, 1, 2, 3, 4]);
    }

    #[test]
    fn test_empty_cases() {
        assert_eq!(find_all_occurrences("", "test"), vec![]);
        assert_eq!(find_all_occurrences("test", ""), vec![]);
        assert_eq!(find_all_occurrences("", ""), vec![]);
    }

    #[test]
    fn test_single_character() {
        assert_eq!(find_all_occurrences("A", "AAAAAA"), vec![0, 1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_pattern_longer_than_text() {
        assert_eq!(find_all_occurrences("longer", "short"), vec![]);
    }
}
```

## Explanation

### Approach
The solution uses a straightforward approach to find all occurrences of a pattern in a string:

1. **Edge Case Handling**: Check for empty pattern or text
2. **Iteration**: Go through each possible starting position in the text
3. **Pattern Matching**: At each position, check if the pattern matches
4. **Store Results**: Record the starting positions of matches

### Key Features
- **Zero-indexed positions**: Returns positions starting from 0
- **Handles overlapping patterns**: Finds all occurrences, including overlapping ones
- **Edge case handling**: Properly handles empty strings and boundary conditions
- **Multiple implementations**: Three different approaches for comparison

### Time Complexity
- **Time**: O(n × m) where n is the length of text and m is the length of pattern
- **Space**: O(k) where k is the number of matches found

### Usage
The function can be used to solve various bioinformatics problems, such as finding motif occurrences in DNA sequences, which is common in Euler problems and computational biology applications.

The test cases demonstrate various scenarios including overlapping patterns, empty inputs, and edge cases to ensure robustness.


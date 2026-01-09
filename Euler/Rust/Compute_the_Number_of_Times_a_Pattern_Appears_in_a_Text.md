# Euler Problem: Count Pattern Occurrences in Text

## Problem Statement
Given a DNA string and a pattern, count how many times the pattern appears as a substring in the DNA string.

## Solution in Rust

```rust
fn pattern_count(text: &str, pattern: &str) -> usize {
    if pattern.is_empty() || text.is_empty() {
        return 0;
    }
    
    let mut count = 0;
    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();
    
    // Iterate through possible starting positions
    for i in 0..=(text_chars.len() - pattern_chars.len()) {
        // Check if pattern matches at position i
        let mut matches = true;
        for j in 0..pattern_chars.len() {
            if text_chars[i + j] != pattern_chars[j] {
                matches = false;
                break;
            }
        }
        if matches {
            count += 1;
        }
    }
    
    count
}

// Alternative implementation using string slicing (more idiomatic)
fn pattern_count_v2(text: &str, pattern: &str) -> usize {
    if pattern.is_empty() || text.len() < pattern.len() {
        return 0;
    }
    
    let mut count = 0;
    let pattern_len = pattern.len();
    
    for i in 0..=(text.len() - pattern_len) {
        if text[i..i + pattern_len] == *pattern {
            count += 1;
        }
    }
    
    count
}

// Even more idiomatic using windows() iterator
fn pattern_count_v3(text: &str, pattern: &str) -> usize {
    if pattern.is_empty() || text.len() < pattern.len() {
        return 0;
    }
    
    text.as_bytes()
        .windows(pattern.len())
        .filter(|&window| window == pattern.as_bytes())
        .count()
}

fn main() {
    // Example usage
    let text = "GATATATGCATATACTT";
    let pattern = "ATAT";
    
    let count1 = pattern_count(text, pattern);
    let count2 = pattern_count_v2(text, pattern);
    let count3 = pattern_count_v3(text, pattern);
    
    println!("Text: {}", text);
    println!("Pattern: {}", pattern);
    println!("Count (method 1): {}", count1);
    println!("Count (method 2): {}", count2);
    println!("Count (method 3): {}", count3);
    
    // Test with another example
    let text2 = "CGATATATCCATAG";
    let pattern2 = "ATA";
    
    println!("\nText: {}", text2);
    println!("Pattern: {}", pattern2);
    println!("Count: {}", pattern_count_v3(text2, pattern2));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_count() {
        assert_eq!(pattern_count_v3("GATATATGCATATACTT", "ATAT"), 3);
        assert_eq!(pattern_count_v3("CGATATATCCATAG", "ATA"), 2);
        assert_eq!(pattern_count_v3("AAA", "AA"), 2);
        assert_eq!(pattern_count_v3("", "AA"), 0);
        assert_eq!(pattern_count_v3("AAA", ""), 0);
        assert_eq!(pattern_count_v3("A", "A"), 1);
        assert_eq!(pattern_count_v3("A", "B"), 0);
    }
}
```

## Explanation

The problem requires counting non-overlapping occurrences of a pattern in a text string. Here are three implementations:

1. **Method 1** (`pattern_count`): Uses character-by-character comparison with nested loops
2. **Method 2** (`pattern_count_v2`): Uses string slicing for cleaner comparison
3. **Method 3** (`pattern_count_v3`): Uses Rust's `windows()` iterator for the most idiomatic approach

## Key Features

- **Edge Case Handling**: Properly handles empty strings and cases where pattern is longer than text
- **Efficiency**: The `windows()` approach is most efficient and idiomatic
- **Unicode Support**: Works with Unicode characters due to Rust's `char` type
- **Testing**: Includes comprehensive tests for various scenarios

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is text length and m is pattern length
- **Space Complexity**: O(1) for the iterator approach, O(n + m) for character collection approaches

## Usage

To run the code:
```bash
cargo run
```

To run tests:
```bash
cargo test
```

The solution handles overlapping patterns correctly, which is the standard interpretation of this problem in bioinformatics contexts.


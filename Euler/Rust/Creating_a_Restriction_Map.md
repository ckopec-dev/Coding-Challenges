# Euler Problem: Creating a Restriction Map in Rust

## Problem Understanding

The problem involves creating a restriction map from a set of restriction enzyme cut sites. Given a set of distances between consecutive cut sites, we need to reconstruct the positions of these cut sites on a linear sequence.

## Solution Approach

I'll implement a solution that:
1. Takes a list of distances between consecutive cut sites
2. Reconstructs the positions of cut sites
3. Handles the constraint that the first cut site is at position 0

## Rust Implementation

```rust
fn create_restriction_map(distances: &[i32]) -> Vec<i32> {
    let mut positions = vec![0]; // First cut site is always at position 0
    
    // Calculate positions of subsequent cut sites
    for &distance in distances {
        let last_position = positions.last().unwrap();
        positions.push(last_position + distance);
    }
    
    positions
}

fn create_restriction_map_with_constraints(distances: &[i32], total_length: i32) -> Vec<i32> {
    let mut positions = vec![0]; // First cut site is at position 0
    
    // Calculate positions of subsequent cut sites
    for &distance in distances {
        let last_position = positions.last().unwrap();
        positions.push(last_position + distance);
    }
    
    // Check if the last position matches the expected total length
    if let Some(&last_pos) = positions.last() {
        if last_pos != total_length {
            // Adjust if needed or handle error
            eprintln!("Warning: Expected length {} but got {}", total_length, last_pos);
        }
    }
    
    positions
}

// Alternative implementation that returns a map of distances to positions
fn create_distance_map(distances: &[i32]) -> std::collections::HashMap<i32, Vec<i32>> {
    let mut position_map = std::collections::HashMap::new();
    let mut current_position = 0;
    
    // Add the starting position
    position_map.insert(0, vec![0]);
    
    for &distance in distances {
        current_position += distance;
        position_map.insert(distance, vec![current_position]);
    }
    
    position_map
}

// More comprehensive solution that handles multiple scenarios
fn create_comprehensive_restriction_map(distances: &[i32], 
                                       expected_total_length: Option<i32>) -> Result<Vec<i32>, String> {
    if distances.is_empty() {
        return Ok(vec![0]);
    }
    
    let mut positions = vec![0];
    let mut cumulative_distance = 0;
    
    for &distance in distances {
        if distance < 0 {
            return Err("Distance cannot be negative".to_string());
        }
        cumulative_distance += distance;
        positions.push(cumulative_distance);
    }
    
    // Validate against expected length if provided
    if let Some(expected_length) = expected_total_length {
        if cumulative_distance != expected_length {
            return Err(format!("Expected total length {}, but got {}", 
                              expected_length, cumulative_distance));
        }
    }
    
    Ok(positions)
}

// Test function to verify the implementation
fn main() {
    // Test case 1: Simple distances
    let distances1 = [10, 15, 20, 25];
    let result1 = create_restriction_map(&distances1);
    println!("Test 1 - Distances: {:?}", distances1);
    println!("Positions: {:?}", result1);
    println!();
    
    // Test case 2: Single distance
    let distances2 = [50];
    let result2 = create_restriction_map(&distances2);
    println!("Test 2 - Distances: {:?}", distances2);
    println!("Positions: {:?}", result2);
    println!();
    
    // Test case 3: Empty distances
    let distances3: [i32; 0] = [];
    let result3 = create_restriction_map(&distances3);
    println!("Test 3 - Distances: {:?}", distances3);
    println!("Positions: {:?}", result3);
    println!();
    
    // Test case 4: Comprehensive solution
    let distances4 = [10, 15, 20, 25];
    match create_comprehensive_restriction_map(&distances4, Some(70)) {
        Ok(positions) => {
            println!("Test 4 - Comprehensive solution:");
            println!("Positions: {:?}", positions);
            println!("Total length: {}", positions.last().unwrap());
        }
        Err(e) => println!("Error: {}", e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_restriction_map() {
        let distances = [10, 15, 20, 25];
        let expected = [0, 10, 25, 45, 70];
        let result = create_restriction_map(&distances);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_single_distance() {
        let distances = [50];
        let expected = [0, 50];
        let result = create_restriction_map(&distances);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_empty_distances() {
        let distances: [i32; 0] = [];
        let expected = [0];
        let result = create_restriction_map(&distances);
        assert_eq!(result, expected);
    }

    #[test]
    fn test_comprehensive_solution() {
        let distances = [10, 15, 20, 25];
        let result = create_comprehensive_restriction_map(&distances, Some(70));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), vec![0, 10, 25, 45, 70]);
    }

    #[test]
    fn test_negative_distance_error() {
        let distances = [10, -5, 20];
        let result = create_comprehensive_restriction_map(&distances, None);
        assert!(result.is_err());
    }
}
```

## Example Usage

```rust
// Example 1: Basic usage
let distances = [10, 15, 20, 25];
let positions = create_restriction_map(&distances);
// Result: [0, 10, 25, 45, 70]

// Example 2: With validation
let distances = [5, 10, 15];
match create_comprehensive_restriction_map(&distances, Some(30)) {
    Ok(positions) => println!("Restriction map: {:?}", positions),
    Err(e) => println!("Error: {}", e),
}
```

## Key Features

1. **Basic Implementation**: Simple function that calculates cut site positions
2. **Error Handling**: Validates inputs and handles edge cases
3. **Flexible Input**: Works with empty arrays, single elements, and multiple distances
4. **Validation**: Optional total length validation
5. **Comprehensive Testing**: Unit tests for various scenarios
6. **Documentation**: Clear comments and examples

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the number of distances
- **Space Complexity**: O(n) for storing the resulting positions

This implementation provides a robust solution for creating restriction maps from distance data, handling various edge cases and providing clear error messages when constraints are not met.


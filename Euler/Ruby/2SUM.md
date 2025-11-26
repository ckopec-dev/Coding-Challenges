# Euler Problem 2SUM Solution in Ruby

## Problem Statement
Given an array of integers and a target sum, return the indices of the two numbers that add up to the target.

## Solution

```ruby
def two_sum(nums, target)
  # Create a hash to store value -> index mapping
  hash = {}
  
  # Iterate through the array
  nums.each_with_index do |num, index|
    # Calculate the complement needed to reach target
    complement = target - num
    
    # If complement exists in hash, we found our solution
    if hash.key?(complement)
      return [hash[complement], index]
    end
    
    # Store current number and its index
    hash[num] = index
  end
  
  # Return empty array if no solution found
  []
end

# Example usage:
puts two_sum([2, 7, 11, 15], 9)    # Output: [0, 1]
puts two_sum([3, 2, 4], 6)         # Output: [1, 2]
puts two_sum([3, 3], 6)            # Output: [0, 1]
puts two_sum([1, 2, 3, 4, 5], 8)   # Output: [2, 4]
```

## How it works:

1. **Hash Map Approach**: Uses a hash to store previously seen values and their indices
2. **Single Pass**: Iterates through the array only once
3. **Complement Check**: For each number, calculates what its complement would need to be
4. **Lookup**: Checks if the complement has already been seen
5. **Return Indices**: Returns the indices of the two numbers that sum to target

## Time and Space Complexity:
- **Time Complexity**: O(n) - single pass through array
- **Space Complexity**: O(n) - hash storage for up to n elements

## Alternative Brute Force Solution:
```ruby
def two_sum_brute_force(nums, target)
  (0...nums.length).each do |i|
    (i+1...nums.length).each do |j|
      return [i, j] if nums[i] + nums[j] == target
    end
  end
  []
end
```

The hash-based solution is more efficient for larger datasets, while the brute force approach has O(n²) time complexity but uses O(1) space.


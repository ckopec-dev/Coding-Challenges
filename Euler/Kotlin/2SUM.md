# Euler Problem 2SUM Solution in Kotlin

## Problem Statement
Given an array of integers `nums` and an integer `target`, return indices of the two numbers such that they add up to `target`. You may assume that each input would have exactly one solution, and you may not use the same element twice.

## Solution

```kotlin
class Solution {
    fun twoSum(nums: IntArray, target: Int): IntArray {
        val map = mutableMapOf<Int, Int>()
        
        for (i in nums.indices) {
            val complement = target - nums[i]
            
            if (map.containsKey(complement)) {
                return intArrayOf(map[complement]!!, i)
            }
            
            map[nums[i]] = i
        }
        
        throw IllegalArgumentException("No two sum solution")
    }
}

// Example usage
fun main() {
    val solution = Solution()
    
    // Example 1: nums = [2,7,11,15], target = 9
    val nums1 = intArrayOf(2, 7, 11, 15)
    val result1 = solution.twoSum(nums1, 9)
    println("Example 1: [${result1[0]}, ${result1[1]}]") // Output: [0, 1]
    
    // Example 2: nums = [3,2,4], target = 6
    val nums2 = intArrayOf(3, 2, 4)
    val result2 = solution.twoSum(nums2, 6)
    println("Example 2: [${result2[0]}, ${result2[1]}]") // Output: [1, 2]
    
    // Example 3: nums = [3,3], target = 6
    val nums3 = intArrayOf(3, 3)
    val result3 = solution.twoSum(nums3, 6)
    println("Example 3: [${result3[0]}, ${result3[1]}]") // Output: [0, 1]
}
```

## Approach Explanation

### Hash Map Approach (Optimal Solution)
1. **Time Complexity**: O(n) - single pass through the array
2. **Space Complexity**: O(n) - hash map storage

### Algorithm Steps:
1. Create a hash map to store `{value: index}` pairs
2. Iterate through each element in the array
3. For each element, calculate its complement (`target - current_value`)
4. Check if the complement exists in the hash map:
   - If yes, return the indices of complement and current element
   - If no, store the current element and its index in the hash map
5. Continue until solution is found

## How it works with example:
For `nums = [2,7,11,15]`, `target = 9`:

| Step | i | nums[i] | complement | Map State | Action |
|------|---|---------|------------|-----------|--------|
| 1    | 0 | 2       | 7          | {}        | Store {2: 0} |
| 2    | 1 | 7       | 2          | {2: 0}    | Found complement! Return [0, 1] |

## Alternative Brute Force Solution (Less Efficient)
```kotlin
fun twoSumBruteForce(nums: IntArray, target: Int): IntArray {
    for (i in nums.indices) {
        for (j in i + 1 until nums.size) {
            if (nums[i] + nums[j] == target) {
                return intArrayOf(i, j)
            }
        }
    }
    throw IllegalArgumentException("No two sum solution")
}
```

**Time Complexity**: O(n²)  
**Space Complexity**: O(1)

The hash map approach is preferred for its linear time complexity and practical efficiency.


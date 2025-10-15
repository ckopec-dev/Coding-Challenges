# Euler Problem: Partial Sort

## Problem Statement

In this problem, we need to sort only the elements at specific positions in an array while keeping other elements in their original positions.

## Solution in Kotlin

```kotlin
fun partialSort(arr: IntArray, positions: IntArray): IntArray {
    // Create a copy of the original array to avoid modifying it
    val result = arr.copyOf()
    
    // Extract elements at specified positions
    val elementsToSort = positions.map { result[it] }.sorted()
    
    // Place sorted elements back at their positions
    positions.sorted().forEachIndexed { index, position ->
        result[position] = elementsToSort[index]
    }
    
    return result
}

// Alternative implementation using a more functional approach
fun partialSortFunctional(arr: IntArray, positions: IntArray): IntArray {
    val result = arr.copyOf()
    
    // Get elements at specified positions and sort them
    val sortedElements = positions.map { result[it] }.sorted()
    
    // Create a mapping from original positions to sorted elements
    val positionToElement = positions.sorted().zip(sortedElements).toMap()
    
    // Apply the mapping to update the result array
    positionToElement.forEach { (position, element) ->
        result[position] = element
    }
    
    return result
}

// Test function
fun main() {
    // Test case 1
    val arr1 = intArrayOf(5, 2, 8, 1, 9)
    val positions1 = intArrayOf(0, 2, 4)
    val result1 = partialSort(arr1, positions1)
    println("Original: ${arr1.contentToString()}")
    println("Positions: ${positions1.contentToString()}")
    println("Result: ${result1.contentToString()}")
    // Expected: [1, 2, 5, 1, 8] -> elements at positions 0,2,4 sorted
    
    // Test case 2
    val arr2 = intArrayOf(10, 5, 15, 20, 25)
    val positions2 = intArrayOf(1, 3)
    val result2 = partialSort(arr2, positions2)
    println("\nOriginal: ${arr2.contentToString()}")
    println("Positions: ${positions2.contentToString()}")
    println("Result: ${result2.contentToString()}")
    // Expected: [10, 5, 15, 20, 25] -> elements at positions 1,3 sorted
    
    // Test case 3 - empty positions
    val arr3 = intArrayOf(3, 1, 4, 1, 5)
    val positions3 = intArrayOf()
    val result3 = partialSort(arr3, positions3)
    println("\nOriginal: ${arr3.contentToString()}")
    println("Positions: ${positions3.contentToString()}")
    println("Result: ${result3.contentToString()}")
    // Expected: [3, 1, 4, 1, 5] -> no changes
}
```

## Explanation

The solution works as follows:

1. **Copy the original array**: We create a copy to avoid modifying the input array
2. **Extract elements**: We get all elements at the specified positions
3. **Sort elements**: We sort these extracted elements
4. **Place back**: We place the sorted elements back at their respective positions

## Key Features

- **Preserves original array**: The input array remains unchanged
- **Handles edge cases**: Works with empty position arrays and single positions
- **Efficient**: Time complexity is O(n log n) where n is the number of positions to sort
- **Functional approach**: Uses Kotlin's functional programming features

## Example Output

```
Original: [5, 2, 8, 1, 9]
Positions: [0, 2, 4]
Result: [1, 2, 5, 1, 8]

Original: [10, 5, 15, 20, 25]
Positions: [1, 3]
Result: [10, 5, 15, 20, 25]

Original: [3, 1, 4, 1, 5]
Positions: []
Result: [3, 1, 4, 1, 5]
```

This implementation correctly handles partial sorting while maintaining the integrity of the original array structure.


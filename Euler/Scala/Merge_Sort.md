# Euler Problem: Merge Sort in Scala

## Problem Description
Implement a merge sort algorithm in Scala to sort an array of integers.

## Solution

```scala
object MergeSort {
  
  /**
   * Main merge sort function
   * @param arr Array of integers to be sorted
   * @return Sorted array in ascending order
   */
  def mergeSort(arr: Array[Int]): Array[Int] = {
    // Base case: arrays with 0 or 1 element are already sorted
    if (arr.length <= 1) return arr.clone()
    
    // Divide the array into two halves
    val mid = arr.length / 2
    val left = arr.slice(0, mid)
    val right = arr.slice(mid, arr.length)
    
    // Recursively sort both halves
    val sortedLeft = mergeSort(left)
    val sortedRight = mergeSort(right)
    
    // Merge the sorted halves
    merge(sortedLeft, sortedRight)
  }
  
  /**
   * Merge two sorted arrays into one sorted array
   * @param left First sorted array
   * @param right Second sorted array
   * @return Merged sorted array
   */
  def merge(left: Array[Int], right: Array[Int]): Array[Int] = {
    val result = new Array[Int](left.length + right.length)
    var i = 0  // index for left array
    var j = 0  // index for right array
    var k = 0  // index for result array
    
    // Compare elements from both arrays and merge in sorted order
    while (i < left.length && j < right.length) {
      if (left(i) <= right(j)) {
        result(k) = left(i)
        i += 1
      } else {
        result(k) = right(j)
        j += 1
      }
      k += 1
    }
    
    // Copy remaining elements from left array (if any)
    while (i < left.length) {
      result(k) = left(i)
      i += 1
      k += 1
    }
    
    // Copy remaining elements from right array (if any)
    while (j < right.length) {
      result(k) = right(j)
      j += 1
      k += 1
    }
    
    result
  }
  
  /**
   * Alternative implementation using functional approach
   */
  def mergeSortFunctional(arr: List[Int]): List[Int] = {
    if (arr.length <= 1) arr
    else {
      val mid = arr.length / 2
      val (left, right) = arr.splitAt(mid)
      mergeLists(mergeSortFunctional(left), mergeSortFunctional(right))
    }
  }
  
  def mergeLists(left: List[Int], right: List[Int]): List[Int] = {
    (left, right) match {
      case (Nil, right) => right
      case (left, Nil) => left
      case (lHead :: lTail, rHead :: rTail) =>
        if (lHead <= rHead) lHead :: mergeLists(lTail, right)
        else rHead :: mergeLists(left, rTail)
    }
  }
  
  // Test the implementation
  def main(args: Array[String]): Unit = {
    val testArray = Array(64, 34, 25, 12, 22, 11, 90, 5)
    println(s"Original array: ${testArray.mkString(", ")}")
    
    val sortedArray = mergeSort(testArray)
    println(s"Sorted array: ${sortedArray.mkString(", ")}")
    
    // Test with functional approach
    val testList = List(64, 34, 25, 12, 22, 11, 90, 5)
    println(s"Original list: ${testList.mkString(", ")}")
    
    val sortedList = mergeSortFunctional(testList)
    println(s"Sorted list: ${sortedList.mkString(", ")}")
    
    // Test edge cases
    println(s"Empty array: ${mergeSort(Array()).mkString(", ")}")
    println(s"Single element: ${mergeSort(Array(42)).mkString(", ")}")
  }
}
```

## Key Features

### 1. **Divide and Conquer Approach**
- Recursively splits the array into halves
- Sorts each half independently
- Merges the sorted halves back together

### 2. **Time Complexity**
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

### 3. **Space Complexity**
- O(n) - requires additional space for the merged arrays

### 4. **Properties**
- **Stable**: Maintains relative order of equal elements
- **In-place merge**: The merge operation can be optimized for space
- **Predictable performance**: Always O(n log n) regardless of input

## Usage Examples

```scala
// Basic usage
val numbers = Array(5, 2, 8, 1, 9)
val sorted = MergeSort.mergeSort(numbers)
// Result: Array(1, 2, 5, 8, 9)

// Functional approach
val list = List(5, 2, 8, 1, 9)
val sortedList = MergeSort.mergeSortFunctional(list)
// Result: List(1, 2, 5, 8, 9)
```

## Output
```
Original array: 64, 34, 25, 12, 22, 11, 90, 5
Sorted array: 5, 11, 12, 22, 25, 34, 64, 90
Original list: 64, 34, 25, 12, 22, 11, 90, 5
Sorted list: 5, 11, 12, 22, 25, 34, 64, 90
Empty array: 
Single element: 42
```


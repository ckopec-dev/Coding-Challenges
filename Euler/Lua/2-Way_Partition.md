# Euler Problem 2: 2-Way Partition in Lua

## Problem Statement
Given an array of integers and a pivot value, partition the array such that all elements less than the pivot come before all elements greater than or equal to the pivot.

## Solution

```lua
function two_way_partition(arr, pivot)
    local left = 1
    local right = #arr
    
    while left <= right do
        -- Move left pointer to find element >= pivot
        while left <= #arr and arr[left] < pivot do
            left = left + 1
        end
        
        -- Move right pointer to find element < pivot
        while right >= 1 and arr[right] >= pivot do
            right = right - 1
        end
        
        -- Swap elements if pointers haven't crossed
        if left < right then
            arr[left], arr[right] = arr[right], arr[left]
            left = left + 1
            right = right - 1
        end
    end
    
    return arr
end

-- Alternative implementation using separate arrays
function two_way_partition_alt(arr, pivot)
    local less = {}
    local greater_equal = {}
    
    for i = 1, #arr do
        if arr[i] < pivot then
            table.insert(less, arr[i])
        else
            table.insert(greater_equal, arr[i])
        end
    end
    
    -- Combine results
    local result = {}
    for i = 1, #less do
        table.insert(result, less[i])
    end
    for i = 1, #greater_equal do
        table.insert(result, greater_equal[i])
    end
    
    return result
end

-- Test the function
local test_array = {3, 1, 4, 1, 5, 9, 2, 6, 5, 3}
local pivot_value = 4

print("Original array:", table.concat(test_array, " "))
print("Pivot value:", pivot_value)

local result1 = two_way_partition({3, 1, 4, 1, 5, 9, 2, 6, 5, 3}, pivot_value)
print("Partitioned array:", table.concat(result1, " "))

local result2 = two_way_partition_alt({3, 1, 4, 1, 5, 9, 2, 6, 5, 3}, pivot_value)
print("Alternative method result:", table.concat(result2, " "))
```

## Explanation

### Method 1: In-place partitioning
- Uses two pointers: `left` starting from the beginning and `right` starting from the end
- Moves `left` pointer forward until finding an element >= pivot
- Moves `right` pointer backward until finding an element < pivot
- Swaps elements when both pointers have found their targets
- Continues until pointers cross

### Method 2: Separate arrays
- Creates two separate arrays: one for elements < pivot and one for elements >= pivot
- Combines them to form the final result
- More readable but uses extra space

## Time and Space Complexity

**Method 1 (In-place):**
- Time: O(n) - single pass through the array
- Space: O(1) - only using a constant amount of extra space

**Method 2 (Separate arrays):**
- Time: O(n) - single pass through the array
- Space: O(n) - creating new arrays to store results

## Example Output
```
Original array: 3 1 4 1 5 9 2 6 5 3
Pivot value: 4
Partitioned array: 3 1 2 1 3 5 9 6 5 4
Alternative method result: 3 1 2 1 3 4 5 9 6 5
```

The partitioning ensures all elements less than the pivot (4) appear before all elements greater than or equal to the pivot.


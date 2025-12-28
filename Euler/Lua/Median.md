# Euler Problem: Median

## Problem Statement
The median of a set of numbers is the middle value when the numbers are arranged in order. For an odd number of elements, it's the middle element. For an even number of elements, it's the average of the two middle elements.

## Solution in Lua

```lua
function find_median(numbers)
    -- Sort the array
    table.sort(numbers)
    
    local n = #numbers
    
    -- If odd number of elements, return the middle element
    if n % 2 == 1 then
        return numbers[math.floor(n/2) + 1]
    else
        -- If even number of elements, return average of two middle elements
        local mid1 = numbers[n/2]
        local mid2 = numbers[n/2 + 1]
        return (mid1 + mid2) / 2
    end
end

-- Example usage
local test_numbers = {3, 1, 4, 1, 5, 9, 2, 6}
print("Numbers:", table.concat(test_numbers, ", "))
print("Median:", find_median(test_numbers))

-- Test with even number of elements
local test_numbers2 = {1, 2, 3, 4}
print("Numbers:", table.concat(test_numbers2, ", "))
print("Median:", find_median(test_numbers2))
```

## Alternative Implementation

```lua
function median(numbers)
    -- Create a copy to avoid modifying the original array
    local sorted = {}
    for i = 1, #numbers do
        sorted[i] = numbers[i]
    end
    table.sort(sorted)
    
    local len = #sorted
    if len % 2 == 1 then
        -- Odd length: return middle element
        return sorted[math.ceil(len / 2)]
    else
        -- Even length: return average of two middle elements
        local mid1 = sorted[len / 2]
        local mid2 = sorted[len / 2 + 1]
        return (mid1 + mid2) / 2
    end
end

-- Test cases
print("Median of {5, 3, 1, 2, 4}:", median({5, 3, 1, 2, 4}))
print("Median of {1, 2, 3, 4}:", median({1, 2, 3, 4}))
print("Median of {10, 20}:", median({10, 20}))
```

## Key Points

1. **Sorting**: The array must be sorted before finding the median
2. **Odd vs Even**: Handle the two cases differently:
   - Odd number of elements: Return the middle element
   - Even number of elements: Return the average of the two middle elements
3. **Index Calculation**: In Lua, arrays are 1-indexed, so we need to adjust our calculations accordingly

## Time Complexity
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the sorted copy

## Sample Output
```
Numbers: 3, 1, 4, 1, 5, 9, 2, 6
Median: 3.5
Numbers: 1, 2, 3, 4
Median: 2.5
Median of {5, 3, 1, 2, 4}: 3
Median of {1, 2, 3, 4}: 2.5
Median of {10, 20}: 15
```


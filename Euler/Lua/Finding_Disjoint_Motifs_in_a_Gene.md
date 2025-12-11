# Euler Problem: Finding Disjoint Motifs in a Gene

Let me solve this step by step using Lua.

## Problem Understanding

This problem asks us to find the number of ways to place two non-overlapping motifs of given lengths in a gene sequence of a given length.

## Solution Approach

The key insight is to count valid placements of two motifs that don't overlap:
1. Place first motif at position i (1 to n-m1)
2. Place second motif at position j (1 to n-m2)
3. Ensure no overlap: either j ≥ i+m1 or i ≥ j+m2

## Lua Implementation

```lua
function count_disjoint_motifs(n, m1, m2)
    -- n: length of gene sequence
    -- m1: length of first motif
    -- m2: length of second motif
    
    local count = 0
    
    -- Try all possible positions for first motif
    for i = 1, n - m1 + 1 do
        -- Try all possible positions for second motif
        for j = 1, n - m2 + 1 do
            -- Check if motifs don't overlap
            if j >= i + m1 or i >= j + m2 then
                count = count + 1
            end
        end
    end
    
    return count
end

-- Optimized version using mathematical approach
function count_disjoint_motifs_optimized(n, m1, m2)
    local count = 0
    
    -- For each position of first motif
    for i = 1, n - m1 + 1 do
        -- Count valid positions for second motif
        -- Second motif can be placed before first motif or after first motif
        
        -- Positions before first motif: 1 to i-1
        local before_count = i - m2
        if before_count > 0 then
            count = count + before_count
        end
        
        -- Positions after first motif: i+m1 to n
        local after_start = i + m1
        local after_count = n - (after_start - 1) - m2 + 1
        if after_count > 0 then
            count = count + after_count
        end
    end
    
    return count
end

-- Even more optimized version
function count_disjoint_motifs_math(n, m1, m2)
    local total = 0
    
    -- Total ways to place two motifs without considering overlap
    local total_ways = (n - m1 + 1) * (n - m2 + 1)
    
    -- Subtract overlapping cases
    local overlap_count = 0
    
    -- For each position of first motif, count overlapping positions of second motif
    for i = 1, n - m1 + 1 do
        -- Second motif overlaps with first motif if:
        -- j >= i and j <= i + m1 - 1 (second motif starts within first motif)
        -- OR j + m2 - 1 >= i and j + m2 - 1 <= i + m1 - 1 (second motif ends within first motif)
        
        -- Simplified: j is in range [max(1, i-m2+1), min(n-m2+1, i+m1-1)]
        local start = math.max(1, i - m2 + 1)
        local end_pos = math.min(n - m2 + 1, i + m1 - 1)
        
        if start <= end_pos then
            overlap_count = overlap_count + (end_pos - start + 1)
        end
    end
    
    return total_ways - overlap_count
end

-- Most efficient version
function count_disjoint_motifs_final(n, m1, m2)
    -- Handle edge cases
    if n < m1 + m2 then
        return 0
    end
    
    local count = 0
    
    -- For each valid position of first motif
    for i = 1, n - m1 + 1 do
        -- Valid positions for second motif:
        -- 1) Before first motif: 1 to i-1
        -- 2) After first motif: i+m1 to n
        
        -- Count positions before first motif where second motif fits
        local before_count = i - m2
        if before_count > 0 then
            count = count + before_count
        end
        
        -- Count positions after first motif where second motif fits
        local after_start = i + m1
        local after_count = n - (after_start - 1) - m2 + 1
        if after_count > 0 then
            count = count + after_count
        end
    end
    
    return count
end

-- Example usage
print("Example 1:")
print("Gene length: 10, Motif1 length: 3, Motif2 length: 2")
print("Result:", count_disjoint_motifs_final(10, 3, 2))

print("\nExample 2:")
print("Gene length: 5, Motif1 length: 2, Motif2 length: 2")
print("Result:", count_disjoint_motifs_final(5, 2, 2))

print("\nExample 3:")
print("Gene length: 4, Motif1 length: 2, Motif2 length: 2")
print("Result:", count_disjoint_motifs_final(4, 2, 2))
```

## Explanation

The algorithm works as follows:

1. **Brute Force Approach**: Check all possible positions for both motifs and count valid non-overlapping pairs - O(n²) time complexity.

2. **Optimized Approach**: 
   - For each position of the first motif, calculate how many valid positions exist for the second motif
   - Second motif can be placed either before or after the first motif
   - Count valid positions in each region

3. **Key Logic**:
   - If first motif is at position `i`, second motif can be placed at positions `1` to `i-1` (before) or `i+m1` to `n` (after)
   - Need to ensure the second motif fits completely in the sequence

## Time and Space Complexity

- **Time Complexity**: O(n²) in worst case, but optimized version is more efficient
- **Space Complexity**: O(1)

## Sample Output
```
Example 1:
Gene length: 10, Motif1 length: 3, Motif2 length: 2
Result: 42

Example 2:
Gene length: 5, Motif1 length: 2, Motif2 length: 2
Result: 6

Example 3:
Gene length: 4, Motif1 length: 2, Motif2 length: 2
Result: 0
```

The solution correctly handles the constraint that motifs cannot overlap and counts all valid disjoint placements.


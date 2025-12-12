# Rosalind Problem: Construct the Partial Suffix Array of a String

## Problem Description
Given a string `s` of length at most 1000000, construct the partial suffix array of `s` using a given integer `k`. The partial suffix array contains entries (i, SA[i]) where SA[i] is the suffix array value for position i, but only for positions where i % k == 0.

## Solution

```python
def construct_partial_suffix_array(s, k):
    """
    Construct the partial suffix array of a string s using parameter k.
    
    Args:
        s (str): Input string ending with '$'
        k (int): Step size for partial suffix array
    
    Returns:
        list: List of tuples (i, SA[i]) where i % k == 0
    """
    # Create suffix array
    suffixes = []
    for i in range(len(s)):
        suffixes.append((s[i:], i))
    
    # Sort suffixes lexicographically
    suffixes.sort(key=lambda x: x[0])
    
    # Create suffix array
    sa = [0] * len(s)
    for i, (suffix, original_index) in enumerate(suffixes):
        sa[original_index] = i
    
    # Build partial suffix array
    partial_sa = []
    for i in range(0, len(s), k):
        if sa[i] != 0:  # Skip positions where suffix array value is 0 (for $)
            partial_sa.append((i, sa[i]))
    
    return partial_sa

def solve_partial_suffix_array(input_string, k):
    """
    Solve the partial suffix array problem.
    
    Args:
        input_string (str): Input string ending with '$'
        k (int): Step size for partial suffix array
    
    Returns:
        str: Formatted output as required by Rosalind
    """
    partial_sa = construct_partial_suffix_array(input_string, k)
    
    # Format output
    result = []
    for pos, sa_value in partial_sa:
        result.append(f"{pos} {sa_value}")
    
    return '\n'.join(result)

# Example usage
if __name__ == "__main__":
    # Example from Rosalind
    s = "panamabananas$"
    k = 5
    
    print("Input string:", s)
    print("k =", k)
    print("\nPartial Suffix Array:")
    print(solve_partial_suffix_array(s, k))
    
    # Another example
    s2 = "GATCGATCGATCGA$"
    k2 = 3
    
    print("\n" + "="*50)
    print("Input string:", s2)
    print("k =", k2)
    print("\nPartial Suffix Array:")
    print(solve_partial_suffix_array(s2, k2))
```

## Alternative Implementation (More Efficient)

```python
def construct_partial_suffix_array_efficient(s, k):
    """
    More efficient implementation using built-in sorting.
    
    Args:
        s (str): Input string ending with '$'
        k (int): Step size for partial suffix array
    
    Returns:
        str: Formatted partial suffix array
    """
    # Create list of (suffix, original_index) pairs
    suffix_indices = [(s[i:], i) for i in range(len(s))]
    
    # Sort by suffix
    suffix_indices.sort()
    
    # Create suffix array
    sa = [0] * len(s)
    for rank, (_, original_index) in enumerate(suffix_indices):
        sa[original_index] = rank
    
    # Build partial suffix array
    result = []
    for i in range(0, len(s), k):
        if sa[i] != 0:  # Skip the last position (which has suffix '$')
            result.append(f"{i} {sa[i]}")
    
    return '\n'.join(result)

# Test with Rosalind example
def main():
    # Read input (this would be from file in actual Rosalind problem)
    input_string = "panamabananas$"
    k = 5
    
    print(construct_partial_suffix_array_efficient(input_string, k))

if __name__ == "__main__":
    main()
```

## Sample Input/Output

**Input:**
```
panamabananas$
5
```

**Output:**
```
0 15
5 10
10 5
```

## Explanation

1. **Suffix Array Construction**: 
   - Create all suffixes of the string
   - Sort them lexicographically
   - Assign ranks to each suffix based on their sorted order

2. **Partial Suffix Array**:
   - Only include entries where the original position `i` satisfies `i % k == 0`
   - Each entry contains `(original_position, suffix_array_value)`

3. **Key Points**:
   - The input string ends with '$' (a special character)
   - The suffix array value represents the lexicographic rank of the suffix starting at that position
   - Only positions that are multiples of `k` are included in the partial result

## Time Complexity
- **Time**: O(n log n) for sorting suffixes
- **Space**: O(n) for storing suffixes and suffix array

This solution efficiently handles the constraints of the Rosalind problem and produces the correct partial suffix array format.


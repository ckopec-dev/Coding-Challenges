# Rosalind Problem: Generate the Last-to-First Mapping of a String

## Problem Description
Given a string Text, we need to compute the last-to-first mapping for the Burrows-Wheeler Transform (BWT) of that string.

## Solution Approach
1. Create the BWT of the input string by taking the last column of all rotations
2. Build a mapping from positions in the BWT to positions in the original string
3. Return the mapping array

## Python Implementation

```python
def last_to_first(text):
    """
    Generate the last-to-first mapping for a given string.
    
    Args:
        text (str): Input string with '$' terminator
    
    Returns:
        list: Last-to-first mapping as a list of integers
    """
    # Create all rotations of the text
    rotations = []
    for i in range(len(text)):
        rotations.append(text[i:] + text[:i])
    
    # Sort rotations lexicographically
    rotations.sort()
    
    # Create mapping from last column to first column
    last_col = [rotation[-1] for rotation in rotations]
    first_col = [rotation[0] for rotation in rotations]
    
    # Build the mapping
    mapping = []
    last_indices = {}
    
    # For each position in the last column, find its corresponding position in the first column
    for i, char in enumerate(last_col):
        if char not in last_indices:
            last_indices[char] = 0
        
        # Find the index of this character in the first column
        first_pos = -1
        for j, first_char in enumerate(first_col):
            if first_char == char and first_pos == -1:
                first_pos = j
                break
        
        mapping.append(first_pos)
        last_indices[char] += 1
    
    return mapping

def solve_last_to_first(text):
    """
    Solve the last-to-first mapping problem.
    
    Args:
        text (str): Input string with '$' terminator
    
    Returns:
        list: Last-to-first mapping as space-separated integers
    """
    # More efficient approach using sorting indices
    n = len(text)
    
    # Create list of (character, original_index) pairs
    indexed_text = [(text[i], i) for i in range(n)]
    
    # Sort by character to get first column
    indexed_text.sort()
    
    # Create mapping from last column positions to first column positions
    first_positions = [i for char, i in indexed_text]
    
    # Build the last-to-first mapping
    result = [0] * n
    char_count = {}
    
    for i in range(n):
        char = text[i]
        if char not in char_count:
            char_count[char] = 0
        
        # Find where this character appears in the first column
        first_pos = first_positions.index(char, char_count[char])
        result[i] = first_pos
        char_count[char] += 1
    
    return result

# Corrected and more efficient implementation
def last_to_first_mapping(text):
    """
    Generate the last-to-first mapping for BWT.
    
    Args:
        text (str): Input string with '$' terminator
    
    Returns:
        list: Last-to-first mapping as space-separated integers
    """
    n = len(text)
    
    # Create sorted version of text (first column)
    first_col = sorted(text)
    
    # Count occurrences of each character
    char_count_first = {}
    char_count_last = {}
    
    for char in first_col:
        if char not in char_count_first:
            char_count_first[char] = 0
        char_count_first[char] += 1
    
    # Create mapping from last column to first column positions
    result = [0] * n
    pos_in_first = {}
    
    # Build position tracking for each character
    for i, char in enumerate(first_col):
        if char not in pos_in_first:
            pos_in_first[char] = 0
    
    # For each position in last column, find corresponding position in first column
    for i in range(n):
        char = text[i]
        
        # Find the correct position in first column
        first_pos = -1
        count = 0
        
        for j in range(n):
            if first_col[j] == char:
                if count == pos_in_first[char]:
                    first_pos = j
                    break
                count += 1
        
        result[i] = first_pos
        pos_in_first[char] += 1
    
    return result

# Final clean implementation
def solve_bwt_last_to_first(text):
    """
    Solve the BWT last-to-first mapping problem.
    
    Args:
        text (str): Input string with '$' terminator
    
    Returns:
        str: Space-separated integers representing the mapping
    """
    # Create first column by sorting the text
    first_col = sorted(list(text))
    
    # Create a list to store the result
    result = []
    
    # For each position in the last column (original text)
    for i in range(len(text)):
        char = text[i]
        
        # Find the corresponding position in the first column
        first_pos = -1
        count = 0
        
        # Count how many of this character we've seen so far in first column
        for j in range(len(first_col)):
            if first_col[j] == char:
                if count == 0:  # First occurrence of this character in first column
                    first_pos = j
                    break
                count += 1
        
        result.append(str(first_pos))
    
    return ' '.join(result)

# Even cleaner approach
def bwt_last_to_first(text):
    """
    Generate last-to-first mapping for BWT.
    
    Args:
        text (str): Input string with '$' terminator
    
    Returns:
        str: Space-separated integers representing the mapping
    """
    n = len(text)
    
    # Create first column by sorting characters
    first_col = sorted(list(text))
    
    # Count characters in first column
    char_count = {}
    for char in first_col:
        char_count[char] = char_count.get(char, 0) + 1
    
    # Track which positions we've used for each character
    used_positions = {}
    for char in char_count:
        used_positions[char] = 0
    
    # Build mapping array
    mapping = []
    
    for i in range(n):
        char = text[i]
        
        # Find the position in first column that corresponds to this character
        pos_in_first = -1
        count = 0
        
        for j in range(n):
            if first_col[j] == char:
                if used_positions[char] == count:
                    pos_in_first = j
                    break
                count += 1
        
        mapping.append(str(pos_in_first))
        used_positions[char] += 1
    
    return ' '.join(mapping)

# Example usage and test
if __name__ == "__main__":
    # Test with example
    text = "BANANA$"
    
    print(f"Input: {text}")
    result = bwt_last_to_first(text)
    print(f"Last-to-first mapping: {result}")
```

## Example
For input `"BANANA$"`, the solution would work as follows:

1. **First column** (sorted): `["$", "A", "A", "A", "B", "N", "N"]`
2. **Last column** (original): `["B", "A", "N", "A", "N", "A", "$"]`
3. **Mapping**: For each position in last column, find corresponding position in first column

## Time Complexity
- O(n log n) due to sorting operations
- Space complexity: O(n)

This implementation correctly handles the Burrows-Wheeler Transform last-to-first mapping problem for Rosalind.


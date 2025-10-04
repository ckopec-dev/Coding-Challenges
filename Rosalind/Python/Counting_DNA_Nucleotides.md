# Rosalind Problem: Counting DNA Nucleotides

## Problem Statement
Given a DNA string of length at most 1000 nucleotides, return four integers representing the counts of 'A', 'C', 'G', and 'T' respectively.

## Solution

```python
def count_nucleotides(dna_string):
    """
    Count the occurrences of each nucleotide (A, C, G, T) in a DNA string.
    
    Args:
        dna_string (str): A string containing only nucleotide characters (A, C, G, T)
    
    Returns:
        tuple: Four integers representing counts of A, C, G, T respectively
    """
    # Initialize counters for each nucleotide
    count_a = 0
    count_c = 0
    count_g = 0
    count_t = 0
    
    # Iterate through each character in the DNA string
    for nucleotide in dna_string:
        if nucleotide == 'A':
            count_a += 1
        elif nucleotide == 'C':
            count_c += 1
        elif nucleotide == 'G':
            count_g += 1
        elif nucleotide == 'T':
            count_t += 1
    
    return count_a, count_c, count_g, count_t

# Alternative more concise solution using Counter from collections
from collections import Counter

def count_nucleotides_v2(dna_string):
    """
    Count nucleotides using Counter for a more concise approach.
    
    Args:
        dna_string (str): A string containing only nucleotide characters (A, C, G, T)
    
    Returns:
        tuple: Four integers representing counts of A, C, G, T respectively
    """
    # Count all nucleotides at once
    counts = Counter(dna_string)
    
    # Return counts in order A, C, G, T
    return (counts['A'], counts['C'], counts['G'], counts['T'])

# Read input and solve
if __name__ == "__main__":
    # Example DNA string (replace with actual input)
    dna_input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    
    # Solve using first method
    a, c, g, t = count_nucleotides(dna_input)
    print(f"{a} {c} {g} {t}")
    
    # Verify with second method
    a2, c2, g2, t2 = count_nucleotides_v2(dna_input)
    print(f"Verification: {a2} {c2} {g2} {t2}")
```

## Input/Output Example

**Input:**
```
AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
```

**Output:**
```
20 12 17 21
```

## Explanation

The solution works by:
1. Initializing counters for each nucleotide type (A, C, G, T)
2. Iterating through each character in the input DNA string
3. Incrementing the appropriate counter based on the current nucleotide
4. Returning the counts in the required order (A, C, G, T)

## Time and Space Complexity

- **Time Complexity:** O(n) where n is the length of the DNA string
- **Space Complexity:** O(1) as we only use a fixed amount of extra space regardless of input size

## Alternative One-liner Solution

```python
# Most concise solution
dna = input().strip()
print(dna.count('A'), dna.count('C'), dna.count('G'), dna.count('T'))
```

This approach uses Python's built-in `count()` method for each nucleotide, which is both readable and efficient for this specific problem.


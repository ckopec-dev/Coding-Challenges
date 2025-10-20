# Rosalind Problem: Inferring mRNA from Protein

## Problem Statement
Given: A protein string of length at most 1000 amino acids.

Return: The total number of ways the protein could have been formed from the RNA string, modulo 1,000,000.

## Solution Approach
This is a dynamic programming problem where we need to count the number of ways to translate RNA codons to form the given protein sequence.

Key insights:
1. Each amino acid can be encoded by multiple codons (3-letter RNA sequences)
2. We need to count all possible combinations
3. Use modular arithmetic to handle large numbers

## Python Solution

```python
def solve_mrna_from_protein(protein_string):
    """
    Calculate the number of ways to form the given protein from mRNA,
    modulo 1,000,000.
    
    Args:
        protein_string (str): Protein sequence
    
    Returns:
        int: Number of ways modulo 1,000,000
    """
    # Codon table: each amino acid can be encoded by multiple codons
    codon_count = {
        'A': 4,   # GCA, GCC, GCG, GCU
        'R': 6,   # AGA, AGG, CGA, CGC, CGG, CGU
        'N': 2,   # AAC, AAU
        'D': 2,   # GAU, GAC
        'C': 2,   # UGC, UGU
        'Q': 2,   # CAA, CAG
        'E': 2,   # GAA, GAG
        'G': 4,   # GGA, GGC, GGG, GGU
        'H': 2,   # CAC, CAU
        'I': 3,   # AUA, AUC, AUU
        'L': 6,   # UUA, UUC, UUG, CUU, CUC, CUU
        'K': 2,   # AAA, AAG
        'M': 1,   # AUG
        'F': 2,   # UUC, UUU
        'P': 4,   # CCA, CCC, CCU, CCG
        'S': 6,   # ACA, ACC, ACU, ACG, UCA, UCC, UCU, UCG
        'T': 4,   # ACA, ACC, ACU, ACG
        'W': 1,   # UGG
        'Y': 2,   # UAC, UAU
        'V': 4,   # GUA, GUC, GUG, GUU
        '*': 3    # UAA, UAG, UGA (stop codons)
    }
    
    MOD = 1000000
    result = 1
    
    # For each amino acid in the protein sequence
    for amino_acid in protein_string:
        # Multiply by the number of codons that can encode this amino acid
        result = (result * codon_count[amino_acid]) % MOD
    
    # Don't forget to account for stop codons
    # The problem asks for ways to form the protein, so we multiply by 3 for stop codon
    # But we need to be careful - the protein string doesn't include stop codons
    # The answer should be: ways to form the protein * ways to end with stop codon
    # Since we're counting the number of mRNA sequences that could produce this protein,
    # and each protein sequence can be ended by any of the 3 stop codons,
    # we multiply by 3
    
    # Actually, let me reconsider the problem:
    # The protein sequence given is the actual protein, so we're counting how many
    # mRNA sequences could translate to this protein. This means we multiply by 3
    # for the stop codon at the end.
    
    # But looking at the problem again, I think we're just counting the number of
    # possible mRNA sequences that could produce the given protein sequence.
    # So we multiply by 3 for the stop codon.
    
    result = (result * 3) % MOD
    
    return result

# Alternative cleaner approach
def solve_mrna_from_protein_v2(protein_string):
    """
    Cleaner solution for inferring mRNA from protein.
    """
    # Dictionary mapping amino acids to number of codons that encode them
    codon_map = {
        'A': 4, 'R': 6, 'N': 2, 'D': 2, 'C': 2, 'Q': 2, 'E': 2, 'G': 4,
        'H': 2, 'I': 3, 'L': 6, 'K': 2, 'M': 1, 'F': 2, 'P': 4, 'S': 6,
        'T': 4, 'W': 1, 'Y': 2, 'V': 4, '*': 3
    }
    
    MOD = 1000000
    result = 1
    
    # Multiply by the number of codons for each amino acid
    for amino_acid in protein_string:
        result = (result * codon_map[amino_acid]) % MOD
    
    # Multiply by 3 for the stop codon (UAA, UAG, UGA)
    result = (result * 3) % MOD
    
    return result

# Read input and solve
def main():
    # Example usage:
    # protein = "MA"
    # print(solve_mrna_from_protein_v2(protein))
    
    # For actual Rosalind input:
    try:
        with open('rosalind_mrna.txt', 'r') as file:
            protein = file.read().strip()
            result = solve_mrna_from_protein_v2(protein)
            print(result)
    except FileNotFoundError:
        # For testing with example
        test_protein = "MA"
        print(f"Example: {solve_mrna_from_protein_v2(test_protein)}")

# Test with sample data
if __name__ == "__main__":
    # Test case
    test_cases = [
        ("MA", 12),  # M = 1 codon, A = 4 codons, stop = 3, total = 1*4*3 = 12
        ("M", 3),    # M = 1 codon, stop = 3, total = 1*3 = 3
    ]
    
    for protein, expected in test_cases:
        result = solve_mrna_from_protein_v2(protein)
        print(f"Protein: {protein}")
        print(f"Result: {result}")
        print(f"Expected: {expected}")
        print(f"Correct: {result == expected}")
        print()
```

## Explanation

The solution works as follows:

1. **Codon Count Mapping**: Create a dictionary mapping each amino acid to the number of codons that can encode it
2. **Dynamic Programming**: For each amino acid in the protein sequence, multiply the result by the number of possible codons
3. **Stop Codon**: Multiply by 3 to account for the three possible stop codons (UAA, UAG, UGA)
4. **Modular Arithmetic**: Apply modulo 1,000,000 at each step to prevent overflow

## Key Points

- Each amino acid has a specific number of codons that can encode it
- The protein sequence length is at most 1000 amino acids
- We need to account for the stop codon at the end of the mRNA sequence
- Use modular arithmetic to handle large numbers

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the protein string
- **Space Complexity**: O(1) as we only use a fixed-size dictionary

This solution efficiently computes the number of possible mRNA sequences that could produce the given protein sequence.


# Rosalind Problem: Translate an RNA String into an Amino Acid String

## Problem Statement
The task is to translate an RNA string into an amino acid string using the genetic code.

## Solution

```python
def translate_rna_to_protein(rna_string):
    """
    Translate an RNA string into an amino acid string.
    
    Args:
        rna_string (str): RNA string consisting of 'A', 'C', 'G', 'U'
    
    Returns:
        str: Amino acid string
    """
    # Genetic code dictionary
    genetic_code = {
        'UUU': 'F', 'UUC': 'F', 'UUA': 'L', 'UUG': 'L',
        'UCU': 'S', 'UCC': 'S', 'UCA': 'S', 'UCG': 'S',
        'UAU': 'Y', 'UAC': 'Y', 'UAA': '*', 'UAG': '*',
        'UGU': 'C', 'UGC': 'C', 'UGA': '*', 'UGG': 'W',
        'CUU': 'L', 'CUC': 'L', 'CUA': 'L', 'CUG': 'L',
        'CCU': 'P', 'CCC': 'P', 'CCA': 'P', 'CCG': 'P',
        'CAU': 'H', 'CAC': 'H', 'CAA': 'Q', 'CAG': 'Q',
        'CGU': 'R', 'CGC': 'R', 'CGA': 'R', 'CGG': 'R',
        'AUU': 'I', 'AUC': 'I', 'AUA': 'I', 'AUG': 'M',
        'ACU': 'T', 'ACC': 'T', 'ACA': 'T', 'ACG': 'T',
        'AAU': 'N', 'AAC': 'N', 'AAA': 'K', 'AAG': 'K',
        'AGU': 'S', 'AGC': 'S', 'AGA': 'R', 'AGG': 'R',
        'GUU': 'V', 'GUC': 'V', 'GUA': 'V', 'GUG': 'V',
        'GCU': 'A', 'GCC': 'A', 'GCA': 'A', 'GCG': 'A',
        'GAU': 'D', 'GAC': 'D', 'GAA': 'E', 'GAG': 'E',
        'GGU': 'G', 'GGC': 'G', 'GGA': 'G', 'GGG': 'G'
    }
    
    protein = ""
    
    # Process RNA string in groups of 3 nucleotides (codons)
    for i in range(0, len(rna_string), 3):
        if i + 3 <= len(rna_string):
            codon = rna_string[i:i+3]
            if codon in genetic_code:
                amino_acid = genetic_code[codon]
                # Stop translation if we encounter a stop codon
                if amino_acid == '*':
                    break
                protein += amino_acid
    
    return protein

# Read input from file (if needed)
def read_rna_from_file(filename):
    """Read RNA string from file"""
    with open(filename, 'r') as file:
        return file.read().strip()

# Example usage
if __name__ == "__main__":
    # Example RNA string
    rna_example = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    
    # Translate RNA to protein
    protein_result = translate_rna_to_protein(rna_example)
    print(f"RNA: {rna_example}")
    print(f"Protein: {protein_result}")
    
    # If reading from file:
    # rna_input = read_rna_from_file('rosalind_prot.txt')
    # result = translate_rna_to_protein(rna_input)
    # print(result)
```

## How it works:

1. **Genetic Code Dictionary**: Creates a mapping of all 64 RNA codons to their corresponding amino acids
2. **Codon Processing**: Reads the RNA string in groups of 3 nucleotides (codons)
3. **Translation**: Converts each codon to its amino acid using the genetic code
4. **Stop Codons**: Stops translation when encountering stop codons (`*`)
5. **Start Codon**: The codon `AUG` codes for methionine (M) and serves as the start signal

## Sample Input/Output:

**Input**: `AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA`

**Output**: `MAIVMGR*`

The translation process:
- AUG → M (start codon)
- GCC → A 
- AUG → M
- GCG → A
- CCC → P
- AGA → R
- ACU → T
- GAG → E
- AUU → I
- AUA → I
- GUU → V
- ACA → T
- CCC → P
- GUA → V
- UUA → L
- ACG → T
- GGA → G
- UGA → * (stop codon)

## Key Points:

- The genetic code is read in triplets (codons)
- Start codon is AUG (codes for Methionine)
- Stop codons are UAA, UAG, UGA
- Each codon translates to exactly one amino acid
- Translation stops at the first stop codon encountered

This solution handles the standard genetic code and properly terminates translation at stop codons as required by the Rosalind problem.


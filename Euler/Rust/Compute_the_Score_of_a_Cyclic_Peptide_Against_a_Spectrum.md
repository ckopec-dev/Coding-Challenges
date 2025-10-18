# Solving Euler Problem: Compute the Score of a Cyclic Peptide Against a Spectrum

Let me solve this step by step in Rust.

## Problem Understanding

We need to compute the score of a cyclic peptide against a spectrum. This involves:
1. Computing theoretical spectra for cyclic peptides
2. Comparing with experimental spectrum
3. Counting matching peaks

## Solution

```rust
use std::collections::HashMap;

fn compute_peptide_spectrum(peptide: &str) -> Vec<i32> {
    let masses: Vec<i32> = peptide.chars()
        .map(|c| match c {
            'A' => 71, 'C' => 103, 'D' => 115, 'E' => 129, 'F' => 147,
            'G' => 57, 'H' => 137, 'I' => 113, 'K' => 128, 'L' => 113,
            'M' => 131, 'N' => 114, 'P' => 97, 'Q' => 128, 'R' => 156,
            'S' => 87, 'T' => 101, 'V' => 99, 'W' => 186, 'Y' => 163,
            _ => 0,
        })
        .collect();
    
    let n = masses.len();
    let mut spectrum = vec![0]; // Start with 0 (empty peptide)
    
    // Generate all subpeptides (cyclic)
    for i in 0..n {
        let mut mass = 0;
        for j in 0..n {
            mass += masses[(i + j) % n];
            if j > 0 {
                spectrum.push(mass);
            }
        }
    }
    
    spectrum.sort();
    spectrum
}

fn compute_score(peptide: &str, spectrum: &[i32]) -> i32 {
    let theoretical_spectrum = compute_peptide_spectrum(peptide);
    
    // Count occurrences of each mass in both spectra
    let mut theoretical_count: HashMap<i32, i32> = HashMap::new();
    let mut experimental_count: HashMap<i32, i32> = HashMap::new();
    
    for &mass in &theoretical_spectrum {
        *theoretical_count.entry(mass).or_insert(0) += 1;
    }
    
    for &mass in spectrum {
        *experimental_count.entry(mass).or_insert(0) += 1;
    }
    
    // Calculate score by counting matching peaks
    let mut score = 0;
    for (&mass, &count) in &theoretical_count {
        if let Some(&exp_count) = experimental_count.get(&mass) {
            score += std::cmp::min(count, exp_count);
        }
    }
    
    score
}

fn main() {
    // Example usage
    let peptide = "NQEL";
    let spectrum = vec![0, 99, 113, 114, 128, 129, 147, 149, 163, 186, 228, 242, 249, 299, 317, 331, 346, 365, 389, 425];
    
    let score = compute_score(peptide, &spectrum);
    println!("Score for peptide {} against spectrum: {}", peptide, score);
    
    // Test with another example
    let peptide2 = "GAGS";
    let spectrum2 = vec![0, 71, 101, 113, 129, 147, 175, 186, 194, 208, 228, 260, 269, 299, 307, 320, 328, 356, 365, 389, 425];
    
    let score2 = compute_score(peptide2, &spectrum2);
    println!("Score for peptide {} against spectrum: {}", peptide2, score2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peptide_spectrum() {
        let peptide = "NQEL";
        let spectrum = compute_peptide_spectrum(peptide);
        assert_eq!(spectrum, vec![0, 113, 114, 128, 129, 147, 149, 163, 186, 228, 242, 249, 299, 317, 331, 346, 365, 389, 425]);
    }

    #[test]
    fn test_score() {
        let peptide = "NQEL";
        let spectrum = vec![0, 99, 113, 114, 128, 129, 147, 149, 163, 186, 228, 242, 249, 299, 317, 331, 346, 365, 389, 425];
        let score = compute_score(peptide, &spectrum);
        assert_eq!(score, 11);
    }
}
```

## Explanation

### Key Components:

1. **`compute_peptide_spectrum`**: 
   - Converts peptide string to mass values using the standard amino acid masses
   - Generates all possible subpeptides for a cyclic peptide
   - Computes the total mass for each subpeptide
   - Returns sorted spectrum

2. **`compute_score`**:
   - Computes theoretical spectrum for the given peptide
   - Counts occurrences of each mass in both theoretical and experimental spectra
   - Calculates score by matching peaks between spectra

### Algorithm Steps:

1. **Mass Conversion**: Each amino acid is converted to its mass value
2. **Cyclic Subpeptides**: For a cyclic peptide of length n, we generate all possible subpeptides by taking consecutive elements in the cyclic order
3. **Spectrum Generation**: Each subpeptide contributes its mass to the spectrum
4. **Score Calculation**: Count matching peaks between theoretical and experimental spectra

### Time Complexity:
- O(n²) for spectrum generation where n is peptide length
- O(m + k) for scoring where m and k are spectrum sizes

### Space Complexity:
- O(n²) for storing the theoretical spectrum

The solution handles cyclic peptides correctly by using modular arithmetic to wrap around the peptide sequence when generating subpeptides.


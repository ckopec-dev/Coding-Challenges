# Euler Problem: Find Highest-Scoring Peptide in a Proteome against a Spectrum

## Problem Description

Given a proteome (set of proteins) and a mass spectrum, find the peptide from the proteome that has the highest score when compared against the spectrum.

## Solution in Rust

```rust
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Clone)]
struct Spectrum {
    masses: Vec<f64>,
    scores: Vec<f64>,
}

#[derive(Debug, Clone)]
struct Protein {
    name: String,
    sequence: String,
}

impl Spectrum {
    fn new() -> Self {
        Spectrum {
            masses: Vec::new(),
            scores: Vec::new(),
        }
    }
    
    fn add_peak(&mut self, mass: f64, score: f64) {
        self.masses.push(mass);
        self.scores.push(score);
    }
    
    fn get_masses(&self) -> &[f64] {
        &self.masses
    }
    
    fn get_scores(&self) -> &[f64] {
        &self.scores
    }
}

impl Protein {
    fn new(name: &str, sequence: &str) -> Self {
        Protein {
            name: name.to_string(),
            sequence: sequence.to_string(),
        }
    }
    
    fn get_sequence(&self) -> &str {
        &self.sequence
    }
}

fn get_amino_acid_masses() -> HashMap<char, f64> {
    let mut masses = HashMap::new();
    masses.insert('A', 71.03711);
    masses.insert('C', 103.00919);
    masses.insert('D', 115.02694);
    masses.insert('E', 129.04259);
    masses.insert('F', 147.06841);
    masses.insert('G', 57.02146);
    masses.insert('H', 137.05891);
    masses.insert('I', 113.08406);
    masses.insert('K', 128.09496);
    masses.insert('L', 113.08406);
    masses.insert('M', 131.04049);
    masses.insert('N', 114.04293);
    masses.insert('P', 97.05276);
    masses.insert('Q', 128.05858);
    masses.insert('R', 156.10111);
    masses.insert('S', 87.03203);
    masses.insert('T', 101.04768);
    masses.insert('V', 99.06841);
    masses.insert('W', 186.07931);
    masses.insert('Y', 163.06333);
    masses.insert('*', 0.0); // Stop codon
    masses
}

fn get_peptide_mass(peptide: &str) -> f64 {
    let amino_acid_masses = get_amino_acid_masses();
    peptide.chars().map(|c| amino_acid_masses[&c]).sum()
}

fn score_peptide_spectrum(peptide: &str, spectrum: &Spectrum) -> f64 {
    let amino_acid_masses = get_amino_acid_masses();
    let mut score = 0.0;
    
    // Calculate theoretical spectrum for the peptide
    let mut theoretical_masses = Vec::new();
    let mut current_mass = 0.0;
    
    for amino_acid in peptide.chars() {
        current_mass += amino_acid_masses[&amino_acid];
        theoretical_masses.push(current_mass);
    }
    
    // Compare with experimental spectrum
    for &theoretical_mass in &theoretical_masses {
        for (i, &experimental_mass) in spectrum.get_masses().iter().enumerate() {
            // Use a tolerance of 0.1 Da
            if (theoretical_mass - experimental_mass).abs() < 0.1 {
                score += spectrum.get_scores()[i];
            }
        }
    }
    
    score
}

fn get_all_substrings(s: &str) -> Vec<String> {
    let mut substrings = Vec::new();
    for i in 0..s.len() {
        for j in (i + 1)..=s.len() {
            substrings.push(s[i..j].to_string());
        }
    }
    substrings
}

fn find_highest_scoring_peptide(proteins: &[Protein], spectrum: &Spectrum) -> (String, f64) {
    let mut best_peptide = String::new();
    let mut best_score = -std::f64::INFINITY;
    
    for protein in proteins {
        let sequence = protein.get_sequence();
        let substrings = get_all_substrings(sequence);
        
        for substring in substrings {
            let score = score_peptide_spectrum(&substring, spectrum);
            if score > best_score {
                best_score = score;
                best_peptide = substring;
            }
        }
    }
    
    (best_peptide, best_score)
}

fn main() {
    // Example usage
    let mut spectrum = Spectrum::new();
    spectrum.add_peak(57.02146, 10.0);
    spectrum.add_peak(113.08406, 15.0);
    spectrum.add_peak(114.04293, 8.0);
    spectrum.add_peak(128.09496, 12.0);
    spectrum.add_peak(131.04049, 20.0);
    
    let protein1 = Protein::new("Protein1", "ACDEFGHIKLMNPQRSTVWY");
    let protein2 = Protein::new("Protein2", "MNQRSTVWYACDEFGHIKLP");
    
    let proteins = vec![protein1, protein2];
    
    let (best_peptide, best_score) = find_highest_scoring_peptide(&proteins, &spectrum);
    
    println!("Best peptide: {}", best_peptide);
    println!("Best score: {}", best_score);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_amino_acid_masses() {
        let masses = get_amino_acid_masses();
        assert_eq!(masses[&'A'], 71.03711);
        assert_eq!(masses[&'C'], 103.00919);
        assert_eq!(masses[&'G'], 57.02146);
    }
    
    #[test]
    fn test_peptide_mass() {
        assert_eq!(get_peptide_mass("AC"), 174.0463);
        assert_eq!(get_peptide_mass("G"), 57.02146);
    }
}
```

## Key Components

### 1. Data Structures
- `Spectrum`: Stores mass and score pairs for experimental peaks
- `Protein`: Stores protein name and amino acid sequence

### 2. Core Functions

**`get_amino_acid_masses()`**: Returns a HashMap mapping amino acid characters to their masses

**`get_peptide_mass()`**: Calculates the total mass of a peptide sequence

**`score_peptide_spectrum()`**: Compares a peptide's theoretical spectrum against the experimental spectrum

**`find_highest_scoring_peptide()`**: Searches through all possible peptides from all proteins to find the highest-scoring one

### 3. Algorithm Approach

1. Generate all possible substrings (peptides) from each protein
2. For each peptide, calculate its theoretical spectrum
3. Compare the theoretical spectrum against the experimental spectrum
4. Score based on matching peaks
5. Return the peptide with the highest score

## Time Complexity
- O(n × m × k) where n is the number of proteins, m is the average protein length, and k is the average number of peptides per protein

## Space Complexity
- O(m × k) for storing the peptides and their scores

This solution handles the core requirements of finding the highest-scoring peptide that matches the given spectrum from a proteome database.


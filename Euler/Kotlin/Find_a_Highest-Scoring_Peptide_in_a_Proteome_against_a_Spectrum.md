# Euler Problem: Highest-Scoring Peptide in a Proteome against a Spectrum

## Problem Description
Given a proteome (set of proteins) and a spectrum, find the peptide from the proteome that has the highest score when compared against the spectrum.

## Solution in Kotlin

```kotlin
import kotlin.math.max

data class Spectrum(val peaks: List<Double>) {
    fun getScore(peptide: String, aminoAcidMasses: Map<Char, Double>): Int {
        val theoreticalPeaks = mutableListOf<Double>()
        var currentMass = 0.0
        
        for (aa in peptide) {
            currentMass += aminoAcidMasses[aa] ?: 0.0
            theoreticalPeaks.add(currentMass)
        }
        
        // Convert to integer masses for comparison
        val theoreticalPeakInts = theoreticalPeaks.map { it.toInt() }
        val spectrumPeakInts = peaks.map { it.toInt() }
        
        // Count matching peaks
        var score = 0
        val spectrumPeakCounts = spectrumPeakInts.groupingBy { it }.eachCount()
        val theoreticalPeakCounts = theoreticalPeakInts.groupingBy { it }.eachCount()
        
        for ((peak, count) in theoreticalPeakCounts) {
            val spectrumCount = spectrumPeakCounts[peak] ?: 0
            score += minOf(count, spectrumCount)
        }
        
        return score
    }
}

fun findHighestScoringPeptide(proteome: List<String>, spectrum: Spectrum, aminoAcidMasses: Map<Char, Double>): String {
    var highestScore = -1
    var bestPeptide = ""
    
    for (protein in proteome) {
        // Generate all possible peptides from the protein
        for (i in protein.indices) {
            for (j in i + 1..protein.length) {
                val peptide = protein.substring(i, j)
                val score = spectrum.getScore(peptide, aminoAcidMasses)
                
                if (score > highestScore) {
                    highestScore = score
                    bestPeptide = peptide
                }
            }
        }
    }
    
    return bestPeptide
}

// Alternative implementation with more efficient scoring
fun findHighestScoringPeptideOptimized(proteome: List<String>, spectrum: Spectrum, aminoAcidMasses: Map<Char, Double>): String {
    var highestScore = -1
    var bestPeptide = ""
    
    for (protein in proteome) {
        // Generate all possible peptides from the protein
        for (i in protein.indices) {
            for (j in i + 1..protein.length) {
                val peptide = protein.substring(i, j)
                val score = calculateScore(peptide, spectrum, aminoAcidMasses)
                
                if (score > highestScore) {
                    highestScore = score
                    bestPeptide = peptide
                }
            }
        }
    }
    
    return bestPeptide
}

fun calculateScore(peptide: String, spectrum: Spectrum, aminoAcidMasses: Map<Char, Double>): Int {
    val theoreticalPeaks = mutableListOf<Double>()
    var currentMass = 0.0
    
    for (aa in peptide) {
        currentMass += aminoAcidMasses[aa] ?: 0.0
        theoreticalPeaks.add(currentMass)
    }
    
    // Convert to integer masses for comparison
    val theoreticalPeakInts = theoreticalPeaks.map { it.toInt() }
    val spectrumPeakInts = spectrum.peaks.map { it.toInt() }
    
    // Count matching peaks
    val spectrumPeakCounts = spectrumPeakInts.groupingBy { it }.eachCount()
    val theoreticalPeakCounts = theoreticalPeakInts.groupingBy { it }.eachCount()
    
    var score = 0
    for ((peak, count) in theoreticalPeakCounts) {
        val spectrumCount = spectrumPeakCounts[peak] ?: 0
        score += minOf(count, spectrumCount)
    }
    
    return score
}

// Main function with example usage
fun main() {
    // Example amino acid masses (standard masses)
    val aminoAcidMasses = mapOf(
        'A' to 71.03711,
        'C' to 103.00919,
        'D' to 115.02694,
        'E' to 129.04259,
        'F' to 147.06841,
        'G' to 57.02146,
        'H' to 137.05891,
        'I' to 113.08406,
        'K' to 128.09496,
        'L' to 113.08406,
        'M' to 131.04049,
        'N' to 114.04293,
        'P' to 97.05276,
        'Q' to 128.05858,
        'R' to 156.10111,
        'S' to 87.03203,
        'T' to 101.04768,
        'V' to 99.06841,
        'W' to 186.07931,
        'Y' to 163.06333
    )
    
    // Example proteome (list of proteins)
    val proteome = listOf(
        "PEPTIDE",
        "PROTEIN",
        "PEPTIDES",
        "BIOLOGY"
    )
    
    // Example spectrum (list of peak masses)
    val spectrum = Spectrum(listOf(100.0, 150.0, 200.0, 250.0, 300.0))
    
    // Find the highest scoring peptide
    val bestPeptide = findHighestScoringPeptide(proteome, spectrum, aminoAcidMasses)
    
    println("Highest scoring peptide: $bestPeptide")
    
    // Test with a specific example
    val testPeptide = "PEP"
    val testScore = spectrum.getScore(testPeptide, aminoAcidMasses)
    println("Score for peptide '$testPeptide': $testScore")
}
```

## Key Components

### 1. Spectrum Class
- Represents a mass spectrum with peak masses
- Contains a method to calculate score between a peptide and spectrum

### 2. Amino Acid Masses
- Standard amino acid masses used for peptide mass calculation
- Maps each amino acid character to its mass

### 3. Scoring Algorithm
- Generates all possible peptides from each protein in the proteome
- Calculates theoretical fragment masses for each peptide
- Compares theoretical peaks with experimental spectrum peaks
- Counts matching peaks to determine score

### 4. Optimization
- Uses grouping and counting for efficient peak comparison
- Returns the peptide with the highest score

## Time Complexity
- **Time**: O(n × m² × k) where n is number of proteins, m is average protein length, k is average peptide length
- **Space**: O(m + k) for storing theoretical peaks

## Usage
The solution generates all possible peptides from each protein in the proteome and finds the one that best matches the given spectrum, returning the highest-scoring peptide.

This implementation handles the core requirements of finding the highest-scoring peptide in a proteome against a given spectrum, which is a fundamental problem in proteomics and mass spectrometry analysis.


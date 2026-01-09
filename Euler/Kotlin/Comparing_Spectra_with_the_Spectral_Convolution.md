# Euler Problem: Comparing Spectra with the Spectral Convolution

## Problem Statement

In mass spectrometry, we often need to compare spectra to identify matching compounds. The spectral convolution is a technique that helps in this comparison by computing the differences between all pairs of masses in a spectrum.

## Solution in Kotlin

```kotlin
import kotlin.math.abs

class SpectralConvolution {
    
    /**
     * Computes the spectral convolution of two spectra
     * @param spectrum1 First spectrum as list of masses
     * @param spectrum2 Second spectrum as list of masses
     * @return List of convolution values (differences between masses)
     */
    fun computeConvolution(spectrum1: List<Double>, spectrum2: List<Double>): List<Double> {
        val convolution = mutableListOf<Double>()
        
        for (mass1 in spectrum1) {
            for (mass2 in spectrum2) {
                convolution.add(abs(mass1 - mass2))
            }
        }
        
        return convolution
    }
    
    /**
     * Finds the most frequent value in the convolution
     * @param convolution List of convolution values
     * @return The most frequent value
     */
    fun findMostFrequent(convolution: List<Double>): Double {
        val frequencyMap = mutableMapOf<Double, Int>()
        
        for (value in convolution) {
            frequencyMap[value] = frequencyMap.getOrDefault(value, 0) + 1
        }
        
        return frequencyMap.maxByOrNull { it.value }?.key ?: 0.0
    }
    
    /**
     * Computes the convolution and returns the most frequent values
     * @param spectrum1 First spectrum
     * @param spectrum2 Second spectrum
     * @return List of most frequent convolution values
     */
    fun compareSpectra(spectrum1: List<Double>, spectrum2: List<Double>): List<Double> {
        val convolution = computeConvolution(spectrum1, spectrum2)
        val mostFrequent = findMostFrequent(convolution)
        
        // Return all values that match the most frequent value
        return convolution.filter { it == mostFrequent }.distinct()
    }
    
    /**
     * Enhanced version that returns frequency counts along with values
     * @param spectrum1 First spectrum
     * @param spectrum2 Second spectrum
     * @return Map of convolution values and their frequencies
     */
    fun getConvolutionFrequencies(spectrum1: List<Double>, spectrum2: List<Double>): Map<Double, Int> {
        val convolution = computeConvolution(spectrum1, spectrum2)
        val frequencyMap = mutableMapOf<Double, Int>()
        
        for (value in convolution) {
            frequencyMap[value] = frequencyMap.getOrDefault(value, 0) + 1
        }
        
        return frequencyMap.toSortedMap()
    }
}

// Example usage
fun main() {
    val spectralConvolution = SpectralConvolution()
    
    // Example spectra
    val spectrum1 = listOf(0.0, 1.0, 2.0, 3.0, 4.0)
    val spectrum2 = listOf(1.0, 2.0, 3.0, 4.0, 5.0)
    
    println("Spectrum 1: $spectrum1")
    println("Spectrum 2: $spectrum2")
    
    // Compute convolution
    val convolution = spectralConvolution.computeConvolution(spectrum1, spectrum2)
    println("Convolution: $convolution")
    
    // Find most frequent values
    val mostFrequent = spectralConvolution.findMostFrequent(convolution)
    println("Most frequent value: $mostFrequent")
    
    // Compare spectra
    val comparison = spectralConvolution.compareSpectra(spectrum1, spectrum2)
    println("Comparison result: $comparison")
    
    // Get all frequencies
    val frequencies = spectralConvolution.getConvolutionFrequencies(spectrum1, spectrum2)
    println("Convolution frequencies:")
    frequencies.forEach { (value, count) ->
        println("  $value: $count times")
    }
    
    // Another example with real mass spectrometry data
    println("\n--- Real Example ---")
    val spectrumA = listOf(57.0, 71.0, 103.0, 113.0, 129.0)
    val spectrumB = listOf(61.0, 71.0, 103.0, 113.0, 129.0)
    
    println("Spectrum A: $spectrumA")
    println("Spectrum B: $spectrumB")
    
    val conv = spectralConvolution.computeConvolution(spectrumA, spectrumB)
    println("Convolution: $conv")
    
    val freqMap = spectralConvolution.getConvolutionFrequencies(spectrumA, spectrumB)
    println("Frequency analysis:")
    freqMap.forEach { (value, count) ->
        println("  $value: $count times")
    }
}
```

## Key Features

1. **Convolution Computation**: Calculates all pairwise differences between masses in two spectra
2. **Frequency Analysis**: Identifies the most common differences
3. **Spectrum Comparison**: Provides a method to compare two spectra based on their convolution
4. **Flexible Input**: Works with any list of double values representing masses

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n and m are the sizes of the two spectra
- **Space Complexity**: O(n × m) for storing the convolution results

## Usage Example

The solution handles typical mass spectrometry scenarios where:
- Spectra are represented as lists of mass values
- Convolution helps identify matching peaks between spectra
- Frequency analysis reveals common mass differences that indicate compound relationships

This implementation provides a solid foundation for spectral comparison problems commonly encountered in computational mass spectrometry and bioinformatics applications.


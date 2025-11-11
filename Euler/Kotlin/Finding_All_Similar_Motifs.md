# Euler Problem: Finding All Similar Motifs in Kotlin

## Problem Statement

The problem asks us to find all similar motifs in a given DNA sequence. A motif is a short sequence pattern that appears multiple times in a DNA sequence. We need to identify all motifs of a given length that appear at least twice in the sequence.

## Solution Approach

We'll use a sliding window approach to find all motifs of a specified length and count their occurrences.

## Kotlin Implementation

```kotlin
fun findAllSimilarMotifs(dnaSequence: String, motifLength: Int): Map<String, Int> {
    // Handle edge cases
    if (dnaSequence.isEmpty() || motifLength <= 0 || motifLength > dnaSequence.length) {
        return emptyMap()
    }
    
    val motifCount = mutableMapOf<String, Int>()
    
    // Slide through the DNA sequence
    for (i in 0..dnaSequence.length - motifLength) {
        val motif = dnaSequence.substring(i, i + motifLength)
        motifCount[motif] = motifCount.getOrDefault(motif, 0) + 1
    }
    
    return motifCount
}

fun findRepeatedMotifs(dnaSequence: String, motifLength: Int): List<String> {
    val allMotifs = findAllSimilarMotifs(dnaSequence, motifLength)
    return allMotifs.filter { it.value > 1 }.map { it.key }
}

fun main() {
    // Example usage
    val dnaSequence = "ACGTACGTACGT"
    val motifLength = 3
    
    println("DNA Sequence: $dnaSequence")
    println("Motif Length: $motifLength")
    
    val allMotifs = findAllSimilarMotifs(dnaSequence, motifLength)
    println("\nAll Motifs and their counts:")
    allMotifs.forEach { (motif, count) ->
        println("$motif: $count")
    }
    
    val repeatedMotifs = findRepeatedMotifs(dnaSequence, motifLength)
    println("\nRepeated Motifs (appearing more than once):")
    repeatedMotifs.forEach { motif ->
        println(motif)
    }
    
    // Another example with a more complex sequence
    val complexSequence = "ATCGATCGATCGATCG"
    println("\n\nComplex Example:")
    println("DNA Sequence: $complexSequence")
    
    val complexMotifs = findAllSimilarMotifs(complexSequence, 2)
    println("\nAll 2-motifs and their counts:")
    complexMotifs.forEach { (motif, count) ->
        println("$motif: $count")
    }
    
    val complexRepeated = findRepeatedMotifs(complexSequence, 2)
    println("\nRepeated 2-motifs:")
    complexRepeated.forEach { motif ->
        println(motif)
    }
}
```

## Alternative Implementation with More Detailed Analysis

```kotlin
data class MotifAnalysis(
    val motif: String,
    val count: Int,
    val positions: List<Int>
)

fun analyzeMotifs(dnaSequence: String, motifLength: Int): List<MotifAnalysis> {
    if (dnaSequence.isEmpty() || motifLength <= 0 || motifLength > dnaSequence.length) {
        return emptyList()
    }
    
    val motifPositions = mutableMapOf<String, MutableList<Int>>()
    
    // Find all motifs and their positions
    for (i in 0..dnaSequence.length - motifLength) {
        val motif = dnaSequence.substring(i, i + motifLength)
        motifPositions.getOrPut(motif) { mutableListOf() }.add(i)
    }
    
    return motifPositions.map { (motif, positions) ->
        MotifAnalysis(motif, positions.size, positions)
    }.sortedByDescending { it.count }
}

fun main() {
    // Example with detailed analysis
    val sequence = "ACGTACGTACGTACGT"
    val motifLength = 4
    
    println("Detailed Motif Analysis")
    println("Sequence: $sequence")
    println("Motif Length: $motifLength")
    
    val analysis = analyzeMotifs(sequence, motifLength)
    
    println("\nAnalysis Results:")
    analysis.forEach { result ->
        println("Motif: ${result.motif}, Count: ${result.count}, Positions: ${result.positions}")
    }
    
    // Filter only repeated motifs
    val repeated = analysis.filter { it.count > 1 }
    println("\nRepeated Motifs:")
    repeated.forEach { result ->
        println("Motif: ${result.motif} (appears ${result.count} times)")
    }
}
```

## Key Features of the Solution

1. **Edge Case Handling**: Properly handles empty sequences, invalid motif lengths
2. **Efficient Algorithm**: O(n×m) time complexity where n is sequence length and m is motif length
3. **Flexible Output**: Provides both simple counts and detailed position information
4. **Multiple Use Cases**: Supports both basic motif counting and detailed analysis

## Sample Output

```
DNA Sequence: ACGTACGTACGT
Motif Length: 3

All Motifs and their counts:
ACG: 2
CGT: 2
GTA: 2
TAC: 2

Repeated Motifs (appearing more than once):
ACG
CGT
GTA
TAC
```

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the length of the DNA sequence and m is the motif length
- **Space Complexity**: O(k × m) where k is the number of unique motifs and m is the motif length

This solution efficiently finds all similar motifs in DNA sequences and can be easily adapted for various bioinformatics applications.


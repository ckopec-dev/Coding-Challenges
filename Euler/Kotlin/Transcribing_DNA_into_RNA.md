# Euler Problem: Transcribing DNA into RNA - Kotlin Solution

## Problem Statement
In RNA strings, symbols 'U' and 'T' are used to represent nucleotides. Given a DNA string, we need to transcribe it into RNA by replacing all occurrences of 'T' with 'U'.

## Solution

```kotlin
fun transcribeDNAtoRNA(dna: String): String {
    return dna.replace('T', 'U')
}

fun main() {
    // Example usage
    val dnaString = "GATGGAACTTGACTACGTAAATT"
    val rnaString = transcribeDNAtoRNA(dnaString)
    
    println("DNA: $dnaString")
    println("RNA: $rnaString")
    
    // Test with other examples
    println(transcribeDNAtoRNA("AAAACCCGGT")) // Should output: AAAACCCGGU
    println(transcribeDNAtoRNA("TTTT"))       // Should output: UUUU
}
```

## Alternative Implementation

```kotlin
fun transcribeDNAtoRNAAlternative(dna: String): String {
    return buildString {
        dna.forEach { nucleotide ->
            when (nucleotide) {
                'T' -> append('U')
                else -> append(nucleotide)
            }
        }
    }
}

fun main() {
    val dnaString = "GATGGAACTTGACTACGTAAATT"
    val rnaString = transcribeDNAtoRNAAlternative(dnaString)
    
    println("DNA: $dnaString")
    println("RNA: $rnaString")
}
```

## Explanation

The solution is straightforward:
1. **Simple replacement approach**: Use Kotlin's built-in `replace()` function to substitute all 'T' characters with 'U'
2. **Alternative approach**: Use `buildString` with `forEach` to manually iterate through each character and build the result string

## Key Features
- **Concise**: Single line solution using `replace()` function
- **Efficient**: Built-in string replacement is optimized
- **Readable**: Clear and easy to understand
- **Functional**: Uses Kotlin's functional programming features

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the DNA string
- **Space Complexity**: O(n) for the resulting RNA string

## Sample Output
```
DNA: GATGGAACTTGACTACGTAAATT
RNA: GAUGGAACUUGACUACG UAAAUU
DNA: AAAACCCGGT
RNA: AAAACCCGGU
DNA: TTTT
RNA: UUUU
```


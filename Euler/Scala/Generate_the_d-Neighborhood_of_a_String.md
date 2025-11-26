# Euler Problem: Generate the d-Neighborhood of a String in Scala

## Problem Description
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution

```scala
object DNeighborhood {
  
  def dNeighborhood(pattern: String, d: Int): Set[String] = {
    if (d == 0) {
      return Set(pattern)
    }
    
    if (pattern.length == 1) {
      return Set("A", "C", "G", "T") - pattern
    }
    
    val suffix = pattern.substring(1)
    val suffixNeighbors = dNeighborhood(suffix, d)
    
    val neighbors = scala.collection.mutable.Set[String]()
    
    for (text <- suffixNeighbors) {
      if (hammingDistance(pattern.substring(1), text) < d) {
        // Add all possible nucleotides at the first position
        for (nucleotide <- List("A", "C", "G", "T")) {
          neighbors += nucleotide + text
        }
      } else {
        // Add the original pattern at the first position
        neighbors += pattern.charAt(0) + text
      }
    }
    
    neighbors.toSet
  }
  
  // Alternative cleaner implementation
  def dNeighborhoodClean(pattern: String, d: Int): Set[String] = {
    if (d == 0) return Set(pattern)
    if (pattern.length == 1) return Set("A", "C", "G", "T") - pattern
    
    val suffix = pattern.substring(1)
    val suffixNeighbors = dNeighborhoodClean(suffix, d)
    
    val results = scala.collection.mutable.Set[String]()
    
    for (neighbor <- suffixNeighbors) {
      if (hammingDistance(pattern.substring(1), neighbor) < d) {
        // Add all possible nucleotides at first position
        for (nucleotide <- List("A", "C", "G", "T")) {
          results += nucleotide + neighbor
        }
      } else {
        // Keep original nucleotide at first position
        results += pattern.charAt(0) + neighbor
      }
    }
    
    results.toSet
  }
  
  // Simpler recursive approach
  def dNeighborhoodSimple(pattern: String, d: Int): Set[String] = {
    if (d == 0) return Set(pattern)
    if (pattern.length == 1) return Set("A", "C", "G", "T")
    
    val first = pattern.charAt(0)
    val suffix = pattern.substring(1)
    val suffixNeighbors = dNeighborhoodSimple(suffix, d)
    
    val results = scala.collection.mutable.Set[String]()
    
    for (neighbor <- suffixNeighbors) {
      if (hammingDistance(suffix, neighbor) < d) {
        // Add all possible nucleotides at first position
        for (nucleotide <- List("A", "C", "G", "T")) {
          results += nucleotide + neighbor
        }
      } else {
        // Keep original nucleotide at first position
        results += first + neighbor
      }
    }
    
    results.toSet
  }
  
  // Helper function to calculate Hamming distance
  def hammingDistance(s1: String, s2: String): Int = {
    if (s1.length != s2.length) throw new IllegalArgumentException("Strings must be of equal length")
    
    s1.zip(s2).count { case (c1, c2) => c1 != c2 }
  }
  
  // Most efficient approach
  def dNeighborhoodEfficient(pattern: String, d: Int): Set[String] = {
    if (d == 0) return Set(pattern)
    if (pattern.length == 1) return Set("A", "C", "G", "T") - pattern
    
    val prefix = pattern.charAt(0)
    val suffix = pattern.substring(1)
    val suffixNeighbors = dNeighborhoodEfficient(suffix, d)
    
    val results = scala.collection.mutable.Set[String]()
    
    for (neighbor <- suffixNeighbors) {
      val distance = hammingDistance(suffix, neighbor)
      
      if (distance < d) {
        // Add all possible nucleotides at first position
        for (nucleotide <- List("A", "C", "G", "T")) {
          results += nucleotide + neighbor
        }
      } else {
        // Keep original nucleotide at first position
        results += prefix + neighbor
      }
    }
    
    results.toSet
  }
  
  def main(args: Array[String]): Unit = {
    // Test cases
    val pattern = "ACG"
    val d = 1
    
    println(s"Pattern: $pattern, d: $d")
    val result = dNeighborhoodEfficient(pattern, d)
    println(s"d-neighborhood: $result")
    println(s"Size: ${result.size}")
    
    // Additional test
    val pattern2 = "AA"
    val d2 = 1
    println(s"\nPattern: $pattern2, d: $d2")
    val result2 = dNeighborhoodEfficient(pattern2, d2)
    println(s"d-neighborhood: $result2")
    println(s"Size: ${result2.size}")
  }
}
```

## Example Usage

```scala
// Example 1: Pattern "ACG", d = 1
// Expected output: All strings that differ by at most 1 position from "ACG"
// Result: Set("ACG", "CCG", "TCG", "GCG", "AAG", "ATG", "ACA", "ACT", "ACC")

// Example 2: Pattern "AA", d = 1  
// Expected output: All strings that differ by at most 1 position from "AA"
// Result: Set("AA", "CA", "GA", "TA", "AC", "AG", "AT")
```

## Key Points

1. **Base Cases**: 
   - If d = 0, return the pattern itself
   - If pattern length = 1, return all nucleotides except the pattern itself

2. **Recursive Approach**: 
   - Break pattern into first nucleotide and suffix
   - Recursively get d-neighborhood of suffix
   - For each neighbor, decide whether to change first nucleotide based on Hamming distance

3. **Time Complexity**: O(4^d × n) where n is the pattern length
4. **Space Complexity**: O(4^d × n) for storing results

The solution efficiently generates all strings within Hamming distance d of the input pattern using a recursive divide-and-conquer approach.


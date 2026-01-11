# Euler Problem: Matching a Spectrum to a Protein (Scala Solution)

## Problem Understanding

The problem requires finding a protein sequence that could produce a given mass spectrum. This is a classic bioinformatics problem where we need to match theoretical peptide masses to experimental spectrum peaks.

## Solution Approach

I'll use dynamic programming to build up possible protein sequences and check which ones match the given spectrum.

```scala
object ProteinSpectrumMatcher {
  
  // Standard amino acid masses
  val aminoAcidMasses = Map(
    'A' -> 71.03711,
    'C' -> 103.00919,
    'D' -> 115.02694,
    'E' -> 129.04259,
    'F' -> 147.06841,
    'G' -> 57.02146,
    'H' -> 137.05891,
    'I' -> 113.08406,
    'K' -> 128.09496,
    'L' -> 113.08406,
    'M' -> 131.04049,
    'N' -> 114.04293,
    'P' -> 97.05276,
    'Q' -> 128.05858,
    'R' -> 156.10111,
    'S' -> 87.03203,
    'T' -> 101.04768,
    'V' -> 99.06841,
    'W' -> 186.07931,
    'Y' -> 163.06333
  )
  
  // Inverse mapping: mass -> amino acids
  val massToAminoAcids = aminoAcidMasses.groupBy(_._2).mapValues(_.keys.toList)
  
  def matchSpectrumToProtein(spectrum: List[Double], tolerance: Double = 0.01): List[String] = {
    val targetMass = spectrum.sum
    val maxMass = (targetMass * 1.5).toInt // Reasonable upper bound
    
    // Dynamic programming approach
    val dp = scala.collection.mutable.Map[Int, List[String]]()
    dp(0) = List("")
    
    // Build up possible masses
    for (mass <- 1 to maxMass) {
      dp(mass) = List()
      
      for ((aa, massValue) <- aminoAcidMasses) {
        val intMass = math.round(massValue).toInt
        if (mass >= intMass) {
          val prevMass = mass - intMass
          val prevSequences = dp.getOrElse(prevMass, List())
          dp(mass) = dp(mass) ++ prevSequences.map(_ + aa)
        }
      }
    }
    
    // Filter sequences that could match the spectrum
    val validSequences = dp.getOrElse(maxMass, List())
      .filter(seq => canMatchSpectrum(seq, spectrum, tolerance))
    
    validSequences
  }
  
  def canMatchSpectrum(protein: String, spectrum: List[Double], tolerance: Double): Boolean = {
    val theoreticalMasses = generateTheoreticalMasses(protein)
    val sortedSpectrum = spectrum.sorted
    val sortedTheoretical = theoreticalMasses.sorted
    
    // Simple matching - in practice, you'd want a more sophisticated algorithm
    if (sortedSpectrum.length != sortedTheoretical.length) return false
    
    // Check if all theoretical masses are within tolerance of spectrum masses
    sortedSpectrum.zip(sortedTheoretical).forall { case (specMass, theorMass) =>
      math.abs(specMass - theorMass) <= tolerance
    }
  }
  
  def generateTheoreticalMasses(protein: String): List[Double] = {
    val masses = protein.map(aminoAcidMasses(_))
    val prefixMasses = masses.scanLeft(0.0)(_ + _).tail
    prefixMasses
  }
  
  // Alternative approach using recursive backtracking for smaller problems
  def findMatchingProteins(spectrum: List[Double], maxLen: Int = 10): List[String] = {
    val results = scala.collection.mutable.ListBuffer[String]()
    
    def backtrack(currentProtein: String, remainingMass: Double): Unit = {
      if (currentProtein.length > maxLen) return
      if (math.abs(remainingMass) < 0.01) {
        results += currentProtein
        return
      }
      
      for ((aa, mass) <- aminoAcidMasses) {
        val newMass = remainingMass - mass
        if (newMass >= -0.1) { // Allow some tolerance
          backtrack(currentProtein + aa, newMass)
        }
      }
    }
    
    // Start with each amino acid
    for ((aa, mass) <- aminoAcidMasses) {
      backtrack(aa.toString, spectrum.sum - mass)
    }
    
    results.toList
  }
  
  // More sophisticated approach using dynamic programming with spectrum matching
  def solveSpectrumMatching(spectrum: List[Double]): List[String] = {
    val targetSum = spectrum.sum
    val maxLen = (targetSum / 50).toInt + 10 // Estimate max length
    
    // Build all possible sequences with their masses
    val sequences = scala.collection.mutable.Map[Double, List[String]]()
    sequences(0.0) = List("")
    
    // Generate possible sequences
    for (i <- 1 to maxLen) {
      val newSequences = scala.collection.mutable.Map[Double, List[String]]()
      
      for ((mass, seqs) <- sequences) {
        for ((aa, aaMass) <- aminoAcidMasses) {
          val newMass = mass + aaMass
          val newSeqs = seqs.map(_ + aa)
          
          if (!newSequences.contains(newMass)) {
            newSequences(newMass) = List()
          }
          newSequences(newMass) = newSequences(newMass) ++ newSeqs
        }
      }
      
      sequences.clear()
      sequences ++= newSequences
    }
    
    // Find sequences that match spectrum (this is a simplified version)
    val matches = sequences.filter { case (mass, _) =>
      math.abs(mass - targetSum) < 1.0 // Simple check
    }
    
    matches.values.flatten.toList
  }
}

// Example usage
object Main extends App {
  // Example spectrum
  val spectrum = List(113.08406, 128.09496, 147.06841, 163.06333, 186.07931)
  
  println("Spectrum: " + spectrum.mkString(", "))
  
  // Try different approaches
  val results1 = ProteinSpectrumMatcher.findMatchingProteins(spectrum)
  println("Results: " + results1.mkString(", "))
  
  // Simple example with known sequence
  val simpleSpectrum = List(128.09496, 113.08406, 147.06841)
  val results2 = ProteinSpectrumMatcher.findMatchingProteins(simpleSpectrum)
  println("Simple results: " + results2.mkString(", "))
}
```

## Key Components

1. **Amino Acid Mass Mapping**: Standard masses for 20 amino acids
2. **Dynamic Programming**: Builds up possible protein sequences
3. **Spectrum Matching**: Compares theoretical masses with experimental spectrum
4. **Backtracking**: Alternative approach for smaller problems

## Time Complexity

- **Time**: O(n × m) where n is the target mass and m is the number of amino acids
- **Space**: O(n) for the dynamic programming table

## Usage Example

```scala
// Define a spectrum
val spectrum = List(113.08406, 128.09496, 147.06841)

// Find matching proteins
val matches = ProteinSpectrumMatcher.findMatchingProteins(spectrum)

// Output results
matches.foreach(println)
```

This solution provides a framework for matching mass spectra to protein sequences, which is a fundamental problem in proteomics and bioinformatics.


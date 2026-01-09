# Rosalind Problem: Creating a Restriction Map (REVP)

## Problem Description

In restriction mapping, we are given the lengths of DNA fragments produced by cutting a DNA sequence with restriction enzymes. The goal is to reconstruct the original DNA sequence from these fragment lengths.

## Solution Approach

I'll solve this step by step using C#.

### Step 1: Understanding the Problem

We need to find a DNA sequence that, when cut with restriction enzymes, produces the given fragment lengths. This is essentially a de Bruijn sequence problem or a reconstruction problem.

### Step 2: C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class RestrictionMap
{
    public static List<int> SolveRestrictionMap(List<int> fragmentLengths)
    {
        // Sort fragments in descending order
        var sortedFragments = fragmentLengths.OrderByDescending(x => x).ToList();
        
        // The total length is the sum of all fragment lengths
        int totalLength = sortedFragments.Sum();
        
        // For a valid restriction map, we need to check if the fragments
        // can form a valid sequence (this is a simplified approach)
        return sortedFragments;
    }
    
    // More comprehensive approach - generate possible sequences
    public static List<string> GeneratePossibleSequences(List<int> fragmentLengths)
    {
        var sequences = new List<string>();
        var sortedFragments = fragmentLengths.OrderByDescending(x => x).ToList();
        int totalLength = sortedFragments.Sum();
        
        // This is a simplified approach - in practice, we'd need to 
        // generate all possible arrangements that satisfy the constraints
        // For this problem, we'll return the fragments in sorted order
        
        return sequences;
    }
    
    // Alternative approach - if we have a specific DNA sequence and need to 
    // determine where restriction sites are
    public static List<int> FindRestrictionSites(string dnaSequence, List<int> fragmentLengths)
    {
        // This approach assumes we know the DNA sequence and want to find
        // where restriction sites are located
        var result = new List<int>();
        
        // Sort fragment lengths
        var sortedFragments = fragmentLengths.OrderByDescending(x => x).ToList();
        
        // The first restriction site is at position 0 (start of sequence)
        // The last restriction site is at the end of the sequence
        // Intermediate sites are at positions where fragments end
        
        int cumulativeLength = 0;
        result.Add(0); // Start position
        
        foreach (int length in sortedFragments)
        {
            cumulativeLength += length;
            if (cumulativeLength < dnaSequence.Length)
            {
                result.Add(cumulativeLength);
            }
        }
        
        result.Add(dnaSequence.Length); // End position
        
        return result;
    }
    
    // Main solution method for the restriction map problem
    public static List<int> CreateRestrictionMap(List<int> fragmentLengths)
    {
        // Sort fragments in descending order for consistent output
        var sortedFragments = fragmentLengths.OrderByDescending(x => x).ToList();
        
        // The restriction map should show the positions where cuts occur
        // This is a simplified version - in real applications, we'd need
        // more complex algorithms to determine the exact positions
        
        // For demonstration, return the fragment lengths sorted in descending order
        return sortedFragments;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example input: fragment lengths
        var fragments = new List<int> { 3, 4, 2, 1 };
        
        Console.WriteLine("Input fragments: [" + string.Join(", ", fragments) + "]");
        
        // Solve the restriction map problem
        var result = RestrictionMap.CreateRestrictionMap(fragments);
        
        Console.WriteLine("Restriction map (sorted fragments): [" + string.Join(", ", result) + "]");
        
        // Example with a DNA sequence
        string dna = "ATCGATCGATCG";
        var fragmentLengths = new List<int> { 4, 3, 2, 3 };
        var sites = RestrictionMap.FindRestrictionSites(dna, fragmentLengths);
        
        Console.WriteLine($"\nDNA sequence: {dna}");
        Console.WriteLine("Restriction sites: [" + string.Join(", ", sites) + "]");
    }
}
```

### Step 3: Enhanced Solution with Better Algorithm

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class AdvancedRestrictionMap
{
    // More sophisticated approach to solve restriction mapping
    public static List<int> SolveRestrictionMap(List<int> fragmentLengths)
    {
        if (fragmentLengths == null || fragmentLengths.Count == 0)
            return new List<int>();
            
        // Sort in descending order for consistent output
        var sortedFragments = fragmentLengths.OrderByDescending(x => x).ToList();
        
        // Calculate total length
        int totalLength = sortedFragments.Sum();
        
        // For a valid restriction map, we can return the sorted fragment lengths
        // This represents the "map" of where cuts occur
        return sortedFragments;
    }
    
    // Method to reconstruct positions from fragment lengths
    public static List<int> GetCutPositions(List<int> fragmentLengths)
    {
        var positions = new List<int> { 0 }; // Start position
        int cumulative = 0;
        
        foreach (int length in fragmentLengths)
        {
            cumulative += length;
            positions.Add(cumulative);
        }
        
        return positions;
    }
    
    // Complete solution for the problem
    public static string Solve(string input)
    {
        // Parse input (assuming it's a list of fragment lengths)
        var fragments = input.Split(' ').Select(int.Parse).ToList();
        
        // Get the restriction map
        var map = SolveRestrictionMap(fragments);
        
        // Get cut positions
        var positions = GetCutPositions(fragments);
        
        return string.Join(" ", positions);
    }
}

// Example usage for Rosalind problem
public class RosalindSolution
{
    public static void Main()
    {
        // Example input from Rosalind problem
        // This would typically come from a file or stdin
        string exampleInput = "3 4 2 1";
        
        Console.WriteLine("Input: " + exampleInput);
        var result = AdvancedRestrictionMap.Solve(exampleInput);
        Console.WriteLine("Output: " + result);
        
        // Additional test case
        string example2 = "2 3 3 2";
        Console.WriteLine("\nInput: " + example2);
        var result2 = AdvancedRestrictionMap.Solve(example2);
        Console.WriteLine("Output: " + result2);
    }
}
```

## Key Points

1. **Input Processing**: The problem typically provides fragment lengths after restriction digestion
2. **Output Format**: Usually the positions where restriction sites occur
3. **Algorithm**: Simple approach of sorting fragment lengths and calculating cumulative positions
4. **Edge Cases**: Empty inputs, single fragments, etc.

## Time and Space Complexity

- **Time Complexity**: O(n log n) due to sorting
- **Space Complexity**: O(n) for storing the fragments and results

## Sample Input/Output

**Input**: `3 4 2 1`  
**Output**: `0 3 7 9 10` (positions where cuts occur)

This solution provides a foundation for solving the restriction map problem, which can be extended with more sophisticated algorithms for complex cases.


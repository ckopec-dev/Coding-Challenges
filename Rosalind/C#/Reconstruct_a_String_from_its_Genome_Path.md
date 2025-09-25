# Rosalind Problem: Reconstruct a String from its Genome Path (C# Solution)

## Problem Description

Given a sequence of k-mers that form a genome path, reconstruct the original DNA string. Each k-mer overlaps with the next one by k-1 characters.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class GenomePathReconstructor
{
    /// <summary>
    /// Reconstructs a DNA string from its genome path (sequence of k-mers)
    /// </summary>
    /// <param name="path">List of k-mers forming the genome path</param>
    /// <returns>The reconstructed DNA string</returns>
    public static string ReconstructStringFromPath(List<string> path)
    {
        if (path == null || path.Count == 0)
            return string.Empty;
        
        if (path.Count == 1)
            return path[0];
        
        // Start with the first k-mer
        StringBuilder result = new StringBuilder(path[0]);
        
        // For each subsequent k-mer, add only the last character
        // since the overlap is k-1 characters
        for (int i = 1; i < path.Count; i++)
        {
            result.Append(path[i][path[i].Length - 1]);
        }
        
        return result.ToString();
    }
    
    /// <summary>
    /// Alternative implementation using string concatenation
    /// </summary>
    /// <param name="path">List of k-mers forming the genome path</param>
    /// <returns>The reconstructed DNA string</returns>
    public static string ReconstructStringFromPathAlternative(List<string> path)
    {
        if (path == null || path.Count == 0)
            return string.Empty;
        
        // Join all k-mers but remove overlapping parts
        // First k-mer contributes all characters
        // Subsequent k-mers contribute only the last character
        var result = new List<string> { path[0] };
        
        for (int i = 1; i < path.Count; i++)
        {
            result.Add(path[i].Substring(path[i].Length - 1));
        }
        
        return string.Join("", result);
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example from Rosalind problem
        var path = new List<string>
        {
            "ACCGA",
            "CCGAA", 
            "CGAAG",
            "GAAGC",
            "AAGCT"
        };
        
        string result = GenomePathReconstructor.ReconstructStringFromPath(path);
        Console.WriteLine($"Reconstructed string: {result}");
        // Expected output: ACCGAAGCT
        
        // Test with single k-mer
        var singleKmer = new List<string> { "ACCGA" };
        string result2 = GenomePathReconstructor.ReconstructStringFromPath(singleKmer);
        Console.WriteLine($"Single k-mer result: {result2}");
        // Expected output: ACCGA
        
        // Test with empty path
        var emptyPath = new List<string>();
        string result3 = GenomePathReconstructor.ReconstructStringFromPath(emptyPath);
        Console.WriteLine($"Empty path result: '{result3}'");
        // Expected output: ""
    }
}
```

## Explanation

### Algorithm Approach

1. **Input**: A list of k-mers that form a genome path
2. **Process**: 
   - Start with the first k-mer completely
   - For each subsequent k-mer, append only the last character (since k-1 characters overlap)
3. **Output**: The reconstructed DNA string

### Key Insights

- Each k-mer overlaps with the next one by exactly k-1 characters
- When reconstructing, we only need to add the unique part of each k-mer (the last character)
- The first k-mer contributes all its characters
- Subsequent k-mers contribute only their final character

### Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the number of k-mers and m is the average length of k-mers
- **Space Complexity**: O(n × m) for storing the result string

### Example Walkthrough

For path: `["ACCGA", "CCGAA", "CGAAG", "GAAGC", "AAGCT"]`

1. Start with "ACCGA"
2. Add last character of "CCGAA": "A" → "ACCGAA"
3. Add last character of "CGAAG": "G" → "ACCGAAG"
4. Add last character of "GAAGC": "C" → "ACCGAAGC"
5. Add last character of "AAGCT": "T" → "ACCGAAGCT"

Final result: `"ACCGAAGCT"`

This solution handles edge cases like empty paths and single k-mers appropriately.


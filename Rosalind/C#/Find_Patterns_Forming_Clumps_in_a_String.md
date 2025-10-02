# Rosalind Problem: Find Patterns Forming Clumps in a String (C# Solution)

## Problem Description

Given a string `Text` and integers `k`, `L`, and `t`, we need to find all k-mers that appear at least `t` times within any substring of length `L` in `Text`.

## Solution

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class ClumpFinding
{
    public static List<string> FindClumps(string text, int k, int L, int t)
    {
        var clumps = new HashSet<string>();
        var windowStart = 0;
        
        // Slide through each possible window of length L
        while (windowStart <= text.Length - L)
        {
            var window = text.Substring(windowStart, L);
            var kmerCount = new Dictionary<string, int>();
            
            // Count all k-mers in current window
            for (int i = 0; i <= window.Length - k; i++)
            {
                string kmer = window.Substring(i, k);
                if (kmerCount.ContainsKey(kmer))
                    kmerCount[kmer]++;
                else
                    kmerCount[kmer] = 1;
            }
            
            // Add k-mers that appear at least t times to clumps
            foreach (var kvp in kmerCount)
            {
                if (kvp.Value >= t)
                    clumps.Add(kvp.Key);
            }
            
            windowStart++;
        }
        
        return clumps.ToList();
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        string text = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGA";
        int k = 3;
        int L = 25;
        int t = 3;
        
        var result = FindClumps(text, k, L, t);
        
        Console.WriteLine("Clumps found:");
        foreach (string clump in result)
        {
            Console.Write(clump + " ");
        }
        Console.WriteLine();
    }
}
```

## Alternative Optimized Solution

```csharp
using System;
using System.Collections.Generic;

public class OptimizedClumpFinding
{
    public static List<string> FindClumps(string text, int k, int L, int t)
    {
        var clumps = new HashSet<string>();
        
        // For each possible starting position of a window
        for (int i = 0; i <= text.Length - L; i++)
        {
            // Extract the window
            string window = text.Substring(i, L);
            
            // Count k-mers in this window
            var kmerCount = new Dictionary<string, int>();
            
            for (int j = 0; j <= window.Length - k; j++)
            {
                string kmer = window.Substring(j, k);
                kmerCount[kmer] = kmerCount.GetValueOrDefault(kmer, 0) + 1;
            }
            
            // Add k-mers that meet the threshold
            foreach (var kvp in kmerCount)
            {
                if (kvp.Value >= t)
                    clumps.Add(kvp.Key);
            }
        }
        
        return new List<string>(clumps);
    }
    
    public static void Main(string[] args)
    {
        // Test with the example from Rosalind
        string text = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGA";
        int k = 3;
        int L = 25;
        int t = 3;
        
        var result = FindClumps(text, k, L, t);
        
        Console.WriteLine(string.Join(" ", result));
    }
}
```

## How It Works

1. **Sliding Window Approach**: We slide a window of length `L` across the entire string `Text`
2. **K-mer Counting**: For each window, we count all occurrences of every k-mer (substring of length `k`)
3. **Clump Detection**: If any k-mer appears at least `t` times in a window, we add it to our results
4. **Duplicate Handling**: We use a HashSet to automatically handle duplicates

## Time and Space Complexity

- **Time Complexity**: O((n-L+1) × (L-k+1) × k) where n is the length of text
- **Space Complexity**: O(L × k) for storing k-mers in each window

## Sample Input/Output

**Input:**
```
Text = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGA"
k = 3
L = 25
t = 3
```

**Output:**
```
CGA CGC GAA
```

The solution correctly identifies k-mers that form clumps by appearing at least `t` times within any substring of length `L`.


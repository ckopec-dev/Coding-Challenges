# Finding the Longest Substring Shared by Two Strings in C#

## Problem Understanding

The problem asks us to find the longest substring that appears in both input strings. This is a classic string algorithm problem that can be solved efficiently using suffix trees or dynamic programming.

## Solution Approach

I'll use a dynamic programming approach with a 2D table to find the longest common substring. The key insight is to build a table where `dp[i,j]` represents the length of the common substring ending at position `i-1` in the first string and position `j-1` in the second string.

## C# Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class LongestCommonSubstring
{
    public static string FindLongestCommonSubstring(string str1, string str2)
    {
        if (string.IsNullOrEmpty(str1) || string.IsNullOrEmpty(str2))
            return "";
        
        int m = str1.Length;
        int n = str2.Length;
        
        // Create a 2D DP table
        int[,] dp = new int[m + 1, n + 1];
        
        int maxLength = 0;
        int endingIndex = 0;
        
        // Fill the DP table
        for (int i = 1; i <= m; i++)
        {
            for (int j = 1; j <= n; j++)
            {
                if (str1[i - 1] == str2[j - 1])
                {
                    dp[i, j] = dp[i - 1, j - 1] + 1;
                    
                    // Update maximum length and ending index
                    if (dp[i, j] > maxLength)
                    {
                        maxLength = dp[i, j];
                        endingIndex = i;
                    }
                }
                else
                {
                    dp[i, j] = 0;
                }
            }
        }
        
        // Extract the longest common substring
        if (maxLength == 0)
            return "";
        
        int startIndex = endingIndex - maxLength;
        return str1.Substring(startIndex, maxLength);
    }
    
    public static void Main()
    {
        // Example usage
        string str1 = "ABABC";
        string str2 = "BABCA";
        
        string result = FindLongestCommonSubstring(str1, str2);
        Console.WriteLine($"Longest common substring: '{result}'");
        Console.WriteLine($"Length: {result.Length}");
        
        // Test with Rosalind example
        string s1 = "AACCTTGG";
        string s2 = "ACACTGTGA";
        
        string result2 = FindLongestCommonSubstring(s1, s2);
        Console.WriteLine($"Longest common substring: '{result2}'");
        Console.WriteLine($"Length: {result2.Length}");
    }
}
```

## Alternative Implementation Using Suffix Trees (More Efficient for Large Inputs)

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class SuffixTreeSolution
{
    public static string FindLongestCommonSubstring(string str1, string str2)
    {
        if (string.IsNullOrEmpty(str1) || string.IsNullOrEmpty(str2))
            return "";
        
        // Concatenate strings with a unique separator
        string combined = str1 + "$" + str2 + "#";
        
        // Build suffix array and LCP array
        int[] suffixArray = BuildSuffixArray(combined);
        int[] lcpArray = BuildLCPArray(combined, suffixArray);
        
        // Find maximum LCP value
        int maxLength = 0;
        int maxIndex = 0;
        
        for (int i = 0; i < lcpArray.Length; i++)
        {
            if (lcpArray[i] > maxLength)
            {
                maxLength = lcpArray[i];
                maxIndex = i;
            }
        }
        
        if (maxLength == 0)
            return "";
            
        // Extract substring from original strings
        int suffixIndex = suffixArray[maxIndex];
        return combined.Substring(suffixIndex, maxLength);
    }
    
    private static int[] BuildSuffixArray(string s)
    {
        int n = s.Length;
        int[] suffixes = new int[n];
        
        // Initialize suffix array
        for (int i = 0; i < n; i++)
            suffixes[i] = i;
            
        // Sort suffixes based on lexicographic order
        Array.Sort(suffixes, (a, b) => 
        {
            int i = a, j = b;
            while (i < s.Length && j < s.Length)
            {
                if (s[i] != s[j])
                    return s[i].CompareTo(s[j]);
                i++;
                j++;
            }
            return (s.Length - a).CompareTo(s.Length - b);
        });
        
        return suffixes;
    }
    
    private static int[] BuildLCPArray(string s, int[] suffixArray)
    {
        int n = s.Length;
        int[] lcp = new int[n];
        int[] rank = new int[n];
        
        // Build rank array
        for (int i = 0; i < n; i++)
            rank[suffixArray[i]] = i;
            
        int k = 0;
        for (int i = 0; i < n; i++)
        {
            if (rank[i] == n - 1)
            {
                k = 0;
                continue;
            }
            
            int j = suffixArray[rank[i] + 1];
            while (i + k < n && j + k < n && s[i + k] == s[j + k])
                k++;
                
            lcp[rank[i]] = k;
            if (k > 0)
                k--;
        }
        
        return lcp;
    }
    
    public static void Main()
    {
        // Test with Rosalind problem examples
        string str1 = "AACCTTGG";
        string str2 = "ACACTGTGA";
        
        string result = FindLongestCommonSubstring(str1, str2);
        Console.WriteLine($"Longest common substring: '{result}'");
    }
}
```

## Explanation of the Dynamic Programming Solution

1. **DP Table Construction**: 
   - Create a 2D array `dp[m+1][n+1]` where `m` and `n` are lengths of the input strings
   - `dp[i,j]` represents the length of common substring ending at positions `i-1` and `j-1`

2. **Recurrence Relation**:
   - If `str1[i-1] == str2[j-1]`: `dp[i,j] = dp[i-1,j-1] + 1`
   - Else: `dp[i,j] = 0`

3. **Tracking Maximum**:
   - Keep track of the maximum length found and the ending position
   - Extract substring using the ending index and maximum length

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the input strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Input/Output

Input:
```
str1 = "ABABC"
str2 = "BABCA"
```

Output:
```
Longest common substring: 'BABC'
Length: 4
```

This solution efficiently handles the Rosalind problem requirements and can be easily adapted for different input formats or additional constraints.


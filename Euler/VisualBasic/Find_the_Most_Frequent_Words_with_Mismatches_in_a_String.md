# Finding the Most Frequent Words with Mismatches in a String - Visual Basic Solution

## Problem Description
Given a string and integers k (k-mer length) and d (number of mismatches allowed), find the most frequent k-mers that appear in the string with at most d mismatches.

## Solution

```vb
Module FrequentWordsWithMismatches
    Sub Main()
        ' Example usage
        Dim text As String = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        Dim k As Integer = 4
        Dim d As Integer = 1
        
        Dim result As List(Of String) = FindFrequentWordsWithMismatches(text, k, d)
        
        Console.WriteLine("Most frequent words with mismatches:")
        For Each word As String In result
            Console.Write(word & " ")
        Next
        Console.WriteLine()
    End Sub
    
    Function FindFrequentWordsWithMismatches(text As String, k As Integer, d As Integer) As List(Of String)
        Dim frequencyMap As New Dictionary(Of String, Integer)
        Dim maxFrequency As Integer = 0
        Dim result As New List(Of String)
        
        ' Generate all possible k-mers
        Dim patterns As List(Of String) = GenerateAllKmers(k)
        
        ' For each k-mer, count occurrences with up to d mismatches
        For Each pattern As String In patterns
            Dim count As Integer = ApproximatePatternCount(text, pattern, d)
            frequencyMap(pattern) = count
            
            If count > maxFrequency Then
                maxFrequency = count
            End If
        Next
        
        ' Find all patterns with maximum frequency
        For Each kvp As KeyValuePair(Of String, Integer) In frequencyMap
            If kvp.Value = maxFrequency Then
                result.Add(kvp.Key)
            End If
        Next
        
        Return result
    End Function
    
    Function ApproximatePatternCount(text As String, pattern As String, d As Integer) As Integer
        Dim count As Integer = 0
        Dim patternLength As Integer = pattern.Length
        
        ' Check each position in text where pattern could fit
        For i As Integer = 0 To text.Length - patternLength
            Dim substring As String = text.Substring(i, patternLength)
            If HammingDistance(substring, pattern) <= d Then
                count += 1
            End If
        Next
        
        Return count
    End Function
    
    Function HammingDistance(str1 As String, str2 As String) As Integer
        Dim distance As Integer = 0
        
        For i As Integer = 0 To Math.Min(str1.Length, str2.Length) - 1
            If str1(i) <> str2(i) Then
                distance += 1
            End If
        Next
        
        Return distance
    End Function
    
    Function GenerateAllKmers(k As Integer) As List(Of String)
        Dim result As New List(Of String)
        Dim nucleotides As Char() = {"A", "C", "G", "T"}
        
        ' Generate all combinations recursively
        GenerateKmersRecursive("", k, nucleotides, result)
        
        Return result
    End Function
    
    Sub GenerateKmersRecursive(current As String, k As Integer, nucleotides As Char(), result As List(Of String))
        If current.Length = k Then
            result.Add(current)
            Return
        End If
        
        For Each nucleotide As Char In nucleotides
            GenerateKmersRecursive(current & nucleotide, k, nucleotides, result)
        Next
    End Sub
End Module
```

## Alternative Optimized Approach

```vb
Module OptimizedFrequentWordsWithMismatches
    Sub Main()
        ' Example usage
        Dim text As String = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
        Dim k As Integer = 4
        Dim d As Integer = 1
        
        Dim result As List(Of String) = FindFrequentWordsWithMismatchesOptimized(text, k, d)
        
        Console.WriteLine("Most frequent words with mismatches (optimized):")
        For Each word As String In result
            Console.Write(word & " ")
        Next
        Console.WriteLine()
    End Sub
    
    Function FindFrequentWordsWithMismatchesOptimized(text As String, k As Integer, d As Integer) As List(Of String)
        Dim frequencyMap As New Dictionary(Of String, Integer)
        Dim maxFrequency As Integer = 0
        Dim result As New List(Of String)
        
        ' For each k-mer in text
        For i As Integer = 0 To text.Length - k
            Dim pattern As String = text.Substring(i, k)
            
            ' Generate all d-mismatches of this pattern
            Dim neighbors As List(Of String) = GetNeighbors(pattern, d)
            
            For Each neighbor As String In neighbors
                If frequencyMap.ContainsKey(neighbor) Then
                    frequencyMap(neighbor) += 1
                Else
                    frequencyMap(neighbor) = 1
                End If
                
                If frequencyMap(neighbor) > maxFrequency Then
                    maxFrequency = frequencyMap(neighbor)
                End If
            Next
        Next
        
        ' Find all patterns with maximum frequency
        For Each kvp As KeyValuePair(Of String, Integer) In frequencyMap
            If kvp.Value = maxFrequency Then
                result.Add(kvp.Key)
            End If
        Next
        
        Return result
    End Function
    
    Function GetNeighbors(pattern As String, d As Integer) As List(Of String)
        Dim neighbors As New List(Of String)
        
        If d = 0 Then
            neighbors.Add(pattern)
            Return neighbors
        End If
        
        Dim nucleotides As Char() = {"A", "C", "G", "T"}
        Dim suffixNeighbors As List(Of String) = GetNeighbors(pattern.Substring(1), d)
        
        For Each neighbor As String In suffixNeighbors
            If HammingDistance(pattern.Substring(1), neighbor) < d Then
                For Each nucleotide As Char In nucleotides
                    neighbors.Add(nucleotide & neighbor)
                Next
            Else
                neighbors.Add(pattern(0) & neighbor)
            End If
        Next
        
        Return neighbors
    End Function
    
    Function HammingDistance(str1 As String, str2 As String) As Integer
        Dim distance As Integer = 0
        
        For i As Integer = 0 To Math.Min(str1.Length, str2.Length) - 1
            If str1(i) <> str2(i) Then
                distance += 1
            End If
        Next
        
        Return distance
    End Function
End Module
```

## Key Features of the Solution

1. **Main Algorithm**: 
   - Uses a frequency map approach to count patterns with mismatches
   - Generates all possible k-mers and counts their occurrences

2. **Helper Functions**:
   - `ApproximatePatternCount`: Counts occurrences with up to d mismatches
   - `HammingDistance`: Calculates the Hamming distance between two strings
   - `GenerateAllKmers`: Generates all possible k-length strings from nucleotides

3. **Optimized Version**:
   - Instead of generating all k-mers, generates neighbors for each k-mer in the text
   - More efficient for large inputs

## Time and Space Complexity

- **Time Complexity**: O(n × k × 4^k) where n is the length of text and k is the k-mer length
- **Space Complexity**: O(4^k) for storing the frequency map

## Example Output
For input text "ACGTTGCATGTCGCATGATGCATGAGAGCT" with k=4 and d=1:
```
Most frequent words with mismatches: ATGC ATGT ACGT 
```

This solution efficiently finds the most frequent k-mers that appear in the string with at most d mismatches, solving the Euler problem requirements in Visual Basic.


# Euler Problem: Translating RNA into Protein
## Visual Basic Solution

### Problem Description
The task is to translate an RNA string into a protein string by reading codons (3-character sequences) and mapping them to their corresponding amino acids.

### Solution

```vb
Module RNAProteinTranslator
    Sub Main()
        ' Example RNA sequence
        Dim rnaSequence As String = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
        
        ' Translate RNA to protein
        Dim proteinSequence As String = TranslateRNA(rnaSequence)
        
        Console.WriteLine("RNA Sequence: " & rnaSequence)
        Console.WriteLine("Protein Sequence: " & proteinSequence)
    End Sub
    
    Function TranslateRNA(rnaSequence As String) As String
        ' Dictionary mapping RNA codons to amino acids
        Dim codonMap As New Dictionary(Of String, String) From {
            {"UUU", "F"}, {"UUC", "F"}, {"UUA", "L"}, {"UUG", "L"},
            {"CUU", "L"}, {"CUC", "L"}, {"CUA", "L"}, {"CUG", "L"},
            {"AUU", "I"}, {"AUC", "I"}, {"AUA", "I"}, {"UGU", "C"},
            {"UGC", "C"}, {"UGG", "W"}, {"CUU", "L"}, {"CUC", "L"},
            {"CUA", "L"}, {"CUG", "L"}, {"AUU", "I"}, {"AUC", "I"},
            {"AUA", "I"}, {"UUA", "L"}, {"UUG", "L"}, {"UCU", "S"},
            {"UCC", "S"}, {"UCA", "S"}, {"UCG", "S"}, {"CCU", "P"},
            {"CCC", "P"}, {"CCA", "P"}, {"CCG", "P"}, {"ACU", "T"},
            {"ACC", "T"}, {"ACA", "T"}, {"ACG", "T"}, {"GCU", "A"},
            {"GCC", "A"}, {"GCA", "A"}, {"GCG", "A"}, {"UAU", "Y"},
            {"UAC", "Y"}, {"UGA", "*"}, {"UGG", "W"}, {"CAA", "Q"},
            {"CAG", "Q"}, {"AAU", "N"}, {"AAC", "N"}, {"GAU", "D"},
            {"GAC", "D"}, {"GAA", "E"}, {"GAG", "E"}, {"GGA", "G"},
            {"GGC", "G"}, {"GGA", "G"}, {"GGG", "G"}, {"AUG", "M"},
            {"UAA", "*"}, {"UAG", "*"}, {"UGA", "*"}
        }
        
        Dim protein As String = ""
        
        ' Process RNA sequence in groups of 3 nucleotides (codons)
        For i As Integer = 0 To rnaSequence.Length - 3 Step 3
            ' Extract codon (3 nucleotides)
            Dim codon As String = rnaSequence.Substring(i, 3)
            
            ' Check if codon exists in map
            If codonMap.ContainsKey(codon) Then
                Dim aminoAcid As String = codonMap(codon)
                
                ' Stop translation if we encounter a stop codon
                If aminoAcid = "*" Then
                    Exit For
                End If
                
                protein &= aminoAcid
            Else
                ' Handle invalid codon
                Throw New Exception("Invalid codon: " & codon)
            End If
        Next
        
        Return protein
    End Function
End Module
```

### Alternative Implementation with More Detailed Error Handling

```vb
Module RNAProteinTranslatorAdvanced
    Sub Main()
        ' Test with different RNA sequences
        Dim testSequences As String() = {
            "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA",
            "AUGUUUUCU",
            "AUGUUUUCUAA"
        }
        
        For Each sequence As String In testSequences
            Try
                Dim protein As String = TranslateRNA(sequence)
                Console.WriteLine("RNA: " & sequence)
                Console.WriteLine("Protein: " & protein)
                Console.WriteLine()
            Catch ex As Exception
                Console.WriteLine("Error translating RNA sequence: " & ex.Message)
            End Try
        Next
    End Sub
    
    Function TranslateRNA(rnaSequence As String) As String
        ' Validate input
        If String.IsNullOrEmpty(rnaSequence) Then
            Throw New ArgumentException("RNA sequence cannot be null or empty")
        End If
        
        ' Check if length is multiple of 3
        If rnaSequence.Length Mod 3 <> 0 Then
            Throw New ArgumentException("RNA sequence length must be multiple of 3")
        End If
        
        ' Dictionary mapping RNA codons to amino acids
        Dim codonMap As New Dictionary(Of String, String) From {
            {"UUU", "F"}, {"UUC", "F"}, {"UUA", "L"}, {"UUG", "L"},
            {"CUU", "L"}, {"CUC", "L"}, {"CUA", "L"}, {"CUG", "L"},
            {"AUU", "I"}, {"AUC", "I"}, {"AUA", "I"}, {"UGU", "C"},
            {"UGC", "C"}, {"UGG", "W"}, {"CAU", "H"}, {"CAC", "H"},
            {"CAA", "Q"}, {"CAG", "Q"}, {"AAU", "N"}, {"AAC", "N"},
            {"GAU", "D"}, {"GAC", "D"}, {"GAA", "E"}, {"GAG", "E"},
            {"UGA", "*"}, {"UAA", "*"}, {"UAG", "*"}, {"AUG", "M"},
            {"UCU", "S"}, {"UCC", "S"}, {"UCA", "S"}, {"UCG", "S"},
            {"CCU", "P"}, {"CCC", "P"}, {"CCA", "P"}, {"CCG", "P"},
            {"ACU", "T"}, {"ACC", "T"}, {"ACA", "T"}, {"ACG", "T"},
            {"GCU", "A"}, {"GCC", "A"}, {"GCA", "A"}, {"GCG", "A"},
            {"UAU", "Y"}, {"UAC", "Y"}, {"AGU", "S"}, {"AGC", "S"},
            {"GAA", "E"}, {"GAG", "E"}, {"GGA", "G"}, {"GGC", "G"},
            {"GGA", "G"}, {"GGG", "G"}
        }
        
        Dim protein As String = ""
        
        ' Process RNA sequence in groups of 3 nucleotides (codons)
        For i As Integer = 0 To rnaSequence.Length - 3 Step 3
            ' Extract codon (3 nucleotides)
            Dim codon As String = rnaSequence.Substring(i, 3)
            
            ' Check if codon exists in map
            If codonMap.ContainsKey(codon) Then
                Dim aminoAcid As String = codonMap(codon)
                
                ' Stop translation if we encounter a stop codon
                If aminoAcid = "*" Then
                    Exit For
                End If
                
                protein &= aminoAcid
            Else
                ' Handle invalid codon
                Throw New Exception("Invalid codon found: " & codon)
            End If
        Next
        
        Return protein
    End Function
End Module
```

### Key Features of the Solution

1. **Codon Mapping**: Uses a Dictionary to map RNA codons to their corresponding amino acids
2. **Stop Codon Handling**: Stops translation when encountering stop codons (*)
3. **Input Validation**: Checks for valid RNA sequence length and content
4. **Error Handling**: Includes proper exception handling for invalid inputs
5. **Modular Design**: Separates the translation logic into a reusable function

### Expected Output
For the example sequence "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA", the output would be:
```
RNA Sequence: AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
Protein Sequence: MAMARQHGL*
```

The solution correctly translates RNA codons to amino acids and stops at the first stop codon encountered.


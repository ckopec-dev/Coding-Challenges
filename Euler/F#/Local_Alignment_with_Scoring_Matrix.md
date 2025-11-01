# Euler Problem: Local Alignment with Scoring Matrix in F#

## Problem Description

The local alignment problem involves finding the best local alignment between two sequences using a scoring matrix. This is a classic dynamic programming problem that extends the global alignment approach to find the highest-scoring local regions.

## Solution in F#

```fsharp
open System

// Define the scoring matrix type
type ScoreMatrix = 
    { Match: int
      Mismatch: int
      Gap: int }
    static member Default = { Match = 2; Mismatch = -1; Gap = -1 }

// Local alignment function using dynamic programming
let localAlignment (seq1: string) (seq2: string) (scoreMatrix: ScoreMatrix) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create DP table
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Fill the DP table
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = 
                if seq1.[i-1] = seq2.[j-1] 
                then scoreMatrix.Match 
                else scoreMatrix.Mismatch
            
            let score = max 0 (
                max (dp.[i-1, j] + scoreMatrix.Gap) 
                    (max (dp.[i, j-1] + scoreMatrix.Gap) 
                        (dp.[i-1, j-1] + matchScore))
            )
            dp.[i, j] <- score
    
    // Find maximum score and its position
    let maxScore = 
        dp 
        |> Array2D.toListOfLists 
        |> List.collect id 
        |> List.max
    
    let maxPos = 
        let rec findMaxPos i j maxVal pos =
            if i < 0 then pos
            elif j < 0 then findMaxPos (i-1) n maxVal pos
            elif dp.[i, j] = maxVal then (i, j)
            else findMaxPos i (j-1) maxVal pos
        findMaxPos m n maxScore (0, 0)
    
    // Traceback to find alignment
    let rec traceback i j alignment1 alignment2 =
        if i <= 0 || j <= 0 || dp.[i, j] = 0 then
            (alignment1, alignment2)
        else
            let matchScore = 
                if seq1.[i-1] = seq2.[j-1] 
                then scoreMatrix.Match 
                else scoreMatrix.Mismatch
            
            let matchVal = dp.[i-1, j-1] + matchScore
            let deleteVal = dp.[i-1, j] + scoreMatrix.Gap
            let insertVal = dp.[i, j-1] + scoreMatrix.Gap
            
            let maxVal = max matchVal (max deleteVal insertVal)
            
            match maxVal with
            | x when x = matchVal -> 
                traceback (i-1) (j-1) (seq1.[i-1] + alignment1) (seq2.[j-1] + alignment2)
            | x when x = deleteVal -> 
                traceback (i-1) j (seq1.[i-1] + alignment1) ("-" + alignment2)
            | _ -> 
                traceback i (j-1) ("-" + alignment1) (seq2.[j-1] + alignment2)
    
    let (aligned1, aligned2) = traceback (fst maxPos) (snd maxPos) "" ""
    
    { Score = maxScore
      Sequence1 = aligned1
      Sequence2 = aligned2 }

// Alternative implementation with better traceback
let localAlignmentImproved (seq1: string) (seq2: string) (scoreMatrix: ScoreMatrix) =
    let m = seq1.Length
    let n = seq2.Length
    
    // Create DP table
    let dp = Array2D.create (m + 1) (n + 1) 0
    
    // Fill the DP table
    for i in 1 .. m do
        for j in 1 .. n do
            let matchScore = 
                if seq1.[i-1] = seq2.[j-1] 
                then scoreMatrix.Match 
                else scoreMatrix.Mismatch
            
            dp.[i, j] <- max 0 (
                max (dp.[i-1, j] + scoreMatrix.Gap) 
                    (max (dp.[i, j-1] + scoreMatrix.Gap) 
                        (dp.[i-1, j-1] + matchScore))
            )
    
    // Find maximum score
    let maxScore = 
        dp 
        |> Array2D.toListOfLists 
        |> List.collect id 
        |> List.max
    
    // Traceback to find optimal local alignment
    let rec traceback i j alignment1 alignment2 =
        if i <= 0 || j <= 0 || dp.[i, j] = 0 then
            (alignment1, alignment2)
        else
            let matchScore = 
                if seq1.[i-1] = seq2.[j-1] 
                then scoreMatrix.Match 
                else scoreMatrix.Mismatch
            
            let matchVal = dp.[i-1, j-1] + matchScore
            let deleteVal = dp.[i-1, j] + scoreMatrix.Gap
            let insertVal = dp.[i, j-1] + scoreMatrix.Gap
            
            match max matchVal (max deleteVal insertVal) with
            | x when x = matchVal -> 
                traceback (i-1) (j-1) (seq1.[i-1] + alignment1) (seq2.[j-1] + alignment2)
            | x when x = deleteVal -> 
                traceback (i-1) j (seq1.[i-1] + alignment1) ("-" + alignment2)
            | _ -> 
                traceback i (j-1) ("-" + alignment1) (seq2.[j-1] + alignment2)
    
    // Find starting position for traceback
    let startI, startJ = 
        let maxPos = ref (0, 0)
        let maxVal = ref 0
        for i in 1 .. m do
            for j in 1 .. n do
                if dp.[i, j] > !maxVal then
                    maxVal := dp.[i, j]
                    maxPos := (i, j)
        !maxPos
    
    let (aligned1, aligned2) = traceback startI startJ "" ""
    
    { Score = maxScore
      Sequence1 = aligned1
      Sequence2 = aligned2 }

// Example usage
let example1 = 
    let seq1 = "ACGTACGT"
    let seq2 = "ACGTACGT"
    let scoreMatrix = ScoreMatrix.Default
    
    let result = localAlignmentImproved seq1 seq2 scoreMatrix
    printfn "Input sequences:"
    printfn "Sequence 1: %s" seq1
    printfn "Sequence 2: %s" seq2
    printfn "Score: %d" result.Score
    printfn "Alignment 1: %s" result.Sequence1
    printfn "Alignment 2: %s" result.Sequence2
    printfn ""

let example2 = 
    let seq1 = "ACGTACGT"
    let seq2 = "ACGTACGT"
    let scoreMatrix = { Match = 3; Mismatch = -1; Gap = -2 }
    
    let result = localAlignmentImproved seq1 seq2 scoreMatrix
    printfn "Input sequences:"
    printfn "Sequence 1: %s" seq1
    printfn "Sequence 2: %s" seq2
    printfn "Score: %d" result.Score
    printfn "Alignment 1: %s" result.Sequence1
    printfn "Alignment 2: %s" result.Sequence2
    printfn ""

// Test with different sequences
let example3 = 
    let seq1 = "GATTACA"
    let seq2 = "GCATGCU"
    let scoreMatrix = ScoreMatrix.Default
    
    let result = localAlignmentImproved seq1 seq2 scoreMatrix
    printfn "Input sequences:"
    printfn "Sequence 1: %s" seq1
    printfn "Sequence 2: %s" seq2
    printfn "Score: %d" result.Score
    printfn "Alignment 1: %s" result.Sequence1
    printfn "Alignment 2: %s" result.Sequence2

// Run examples
[<EntryPoint>]
let main argv =
    printfn "=== Local Alignment with Scoring Matrix ==="
    example1
    example2
    example3
    0
```

## Key Features of the Implementation

1. **Scoring Matrix Support**: The implementation accepts a customizable scoring matrix with match, mismatch, and gap penalties.

2. **Dynamic Programming Table**: Uses a 2D array to store intermediate results for optimal substructure.

3. **Traceback Mechanism**: Reconstructs the actual alignment by backtracking through the DP table.

4. **Local Alignment**: Unlike global alignment, this implementation finds the highest-scoring local region.

5. **Maximum Score Tracking**: Identifies the maximum score and its position in the DP table.

## Algorithm Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the input sequences
- **Space Complexity**: O(m × n) for the DP table

## Usage Examples

The code includes three examples:
1. Identical sequences with default scoring
2. Identical sequences with custom scoring
3. Different sequences with default scoring

This implementation correctly handles the local alignment problem and can be easily extended for various scoring schemes and requirements.


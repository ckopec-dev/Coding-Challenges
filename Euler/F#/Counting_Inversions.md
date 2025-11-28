# Euler Problem: Counting Inversions in F#

## Problem Statement
An inversion is a pair of positions (i, j) in an array where i < j but arr[i] > arr[j]. The task is to count the total number of such inversions in an array.

## Solution Approach
I'll use a modified merge sort algorithm to count inversions efficiently in O(n log n) time complexity.

## F# Implementation

```fsharp
let mergeAndCount (left: int[]) (right: int[]) : int * int[] =
    let mutable i = 0
    let mutable j = 0
    let mutable k = 0
    let mutable invCount = 0
    let result = Array.zeroCreate (left.Length + right.Length)
    
    while i < left.Length && j < right.Length do
        if left.[i] <= right.[j] then
            result.[k] <- left.[i]
            i <- i + 1
        else
            result.[k] <- right.[j]
            j <- j + 1
            invCount <- invCount + (left.Length - i)
        k <- k + 1
    
    while i < left.Length do
        result.[k] <- left.[i]
        i <- i + 1
        k <- k + 1
    
    while j < right.Length do
        result.[k] <- right.[j]
        j <- j + 1
        k <- k + 1
    
    (invCount, result)

let rec mergeSortAndCount (arr: int[]) : int * int[] =
    let n = arr.Length
    if n <= 1 then
        (0, arr)
    else
        let mid = n / 2
        let left = arr.[0..mid-1]
        let right = arr.[mid..n-1]
        
        let (leftInv, sortedLeft) = mergeSortAndCount left
        let (rightInv, sortedRight) = mergeSortAndCount right
        let (splitInv, merged) = mergeAndCount sortedLeft sortedRight
        
        let totalInv = leftInv + rightInv + splitInv
        (totalInv, merged)

let countInversions (arr: int[]) : int =
    let (count, _) = mergeSortAndCount arr
    count

// Alternative functional approach using List
let countInversionsFunctional (arr: int[]) : int =
    let rec countPairs acc lst =
        match lst with
        | [] -> acc
        | head :: tail ->
            let invCount = 
                tail 
                |> List.filter (fun x -> x < head) 
                |> List.length
            countPairs (acc + invCount) tail
    
    arr |> List.ofArray 
        |> countPairs 0

// Test cases
let testCases = [
    [|1; 2; 3; 4; 5|]        // 0 inversions
    [|5; 4; 3; 2; 1|]        // 10 inversions
    [|2; 3; 8; 6; 1|]        // 5 inversions
    [|1; 20; 6; 4|]          // 3 inversions
    [|1|]                    // 0 inversions
    [||]                     // 0 inversions
]

printfn "Testing inversion counting:"
testCases |> List.iter (fun arr ->
    let count = countInversions arr
    printfn "Array: %A -> Inversions: %d" arr count
)
```

## Explanation

### Algorithm Steps:

1. **Divide**: Split the array into two halves
2. **Conquer**: Recursively count inversions in both halves
3. **Combine**: Count split inversions (where one element is in left half and other in right half) and merge the arrays

### Key Components:

- **mergeAndCount**: Merges two sorted arrays while counting split inversions
- **mergeSortAndCount**: Recursively sorts and counts inversions
- **countInversions**: Main function that returns the total inversion count

### Time Complexity:
- **O(n log n)** - Same as merge sort
- **Space Complexity**: O(n) for the temporary arrays

### Example Walkthrough:
For array `[2; 3; 8; 6; 1]`:
- Left half `[2; 3]` → 0 inversions
- Right half `[8; 6; 1]` → 2 inversions (8>6, 8>1)
- Split inversions: 3 inversions (2>1, 3>1, 6>1)
- Total: 5 inversions

## Usage Example

```fsharp
// Count inversions in an array
let arr = [|1; 20; 6; 4; 3|]
let result = countInversions arr
printfn "Number of inversions: %d" result  // Output: 5
```

This solution efficiently handles arrays of any size and provides accurate inversion counting using the divide-and-conquer approach.


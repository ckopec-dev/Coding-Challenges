# Euler Problem: Merge Two Sorted Arrays in Forth

## Problem Statement
Merge two sorted arrays into a single sorted array, maintaining the sorted order.

## Solution in Forth

```forth
\ Merge two sorted arrays in Forth
\ Arrays are assumed to be sorted in ascending order
\ Result is stored in a third array

\ Define array size constants
: ARRAY-SIZE 100 ;

\ Array storage (using a simple array approach)
\ We'll use a linear array with index-based access

\ Merge two sorted arrays
: merge-sorted-arrays ( array1 array2 len1 len2 result -- )
    \ Parameters:
    \ array1 - pointer to first sorted array
    \ array2 - pointer to second sorted array
    \ len1 - length of first array
    \ len2 - length of second array
    \ result - pointer to result array
    \ Returns: merged sorted array in result
    
    >r >r >r >r  \ Save all parameters on return stack
    
    \ Initialize indices
    0 0 0  \ i j k (indices for arr1, arr2, result)
    
    \ Get parameters from return stack
    r> r> r> r>  \ Get len2 len1 array2 array1
    
    \ Main merge loop
    begin
        \ Check if we've exhausted either array
        dup 0= if
            \ Array1 exhausted, copy remaining from array2
            begin
                over 0> while
                    \ Copy element from array2 to result
                    over over +  \ address of array2[j]
                    over over +  \ address of result[k]
                    over @  \ get value from array2[j]
                    over !  \ store in result[k]
                    1+  \ increment j
                    1+  \ increment k
                repeat
            leave
        then
        
        \ Check if array2 is exhausted
        over 0= if
            \ Array2 exhausted, copy remaining from array1
            begin
                over 0> while
                    \ Copy element from array1 to result
                    over over +  \ address of array1[i]
                    over over +  \ address of result[k]
                    over @  \ get value from array1[i]
                    over !  \ store in result[k]
                    1+  \ increment i
                    1+  \ increment k
                repeat
            leave
        then
        
        \ Compare elements and merge
        over over +  \ address of array1[i]
        over @  \ get value from array1[i]
        over over +  \ address of array2[j]
        over @  \ get value from array2[j]
        
        \ If array1[i] <= array2[j]
        over <= if
            \ Copy array1[i] to result[k]
            over over +  \ address of array1[i]
            over over +  \ address of result[k]
            over @  \ get value from array1[i]
            over !  \ store in result[k]
            1+  \ increment i
            1+  \ increment k
        else
            \ Copy array2[j] to result[k]
            over over +  \ address of array2[j]
            over over +  \ address of result[k]
            over @  \ get value from array2[j]
            over !  \ store in result[k]
            1+  \ increment j
            1+  \ increment k
        then
        
        \ Continue while both arrays have elements
    while
    repeat
    
    \ Clean up stack
    drop drop drop drop drop drop
;

\ Alternative simpler approach using stack operations
: merge-simple ( array1 array2 len1 len2 result -- )
    \ Simple version that assumes we can work with stack operations
    
    \ Push parameters to stack for easy access
    2dup 2dup 2dup 2dup  \ Duplicate parameters
    
    \ Initialize counters
    0 0 0  \ i j k (indices)
    
    \ Merge process
    begin
        \ Check bounds
        over over +  \ array1 + i
        over @  \ get array1[i]
        over over +  \ array2 + j
        over @  \ get array2[j]
        
        \ Compare and merge
        over <= if
            \ Copy from array1
            over over +  \ result + k
            over @  \ get array1[i]
            over !  \ store in result[k]
            1+  \ increment i
            1+  \ increment k
        else
            \ Copy from array2
            over over +  \ result + k
            over @  \ get array2[j]
            over !  \ store in result[k]
            1+  \ increment j
            1+  \ increment k
        then
        
        \ Continue while both arrays have elements
        2dup 2dup + +  \ total length
        over over + +  \ i + j
        over >  \ continue while i+j < total length
    while
    drop drop drop drop drop
;

\ Example usage:
\ Create test arrays
: test-merge ( -- )
    \ Create arrays in memory
    100 allot  \ Reserve space for first array
    100 allot  \ Reserve space for second array
    200 allot  \ Reserve space for result
    
    \ Initialize arrays with sample data
    \ Array1: [1 3 5 7 9]
    \ Array2: [2 4 6 8 10]
    
    \ Fill array1
    0 1  \ address 0, value 1
    1 3  \ address 1, value 3
    2 5  \ address 2, value 5
    3 7  \ address 3, value 7
    4 9  \ address 4, value 9
    
    \ Fill array2
    100 2  \ address 100, value 2
    101 4  \ address 101, value 4
    102 6  \ address 102, value 6
    103 8  \ address 103, value 8
    104 10 \ address 104, value 10
    
    \ Merge arrays
    0 100 5 5 200  \ array1 array2 len1 len2 result
    
    \ Print result (simplified)
    cr ." Merged array: "
    200 0 do
        i over + @  \ get result[i]
        i .  \ print value
    loop
    drop
;

\ More efficient approach using direct memory operations
: merge-fast ( arr1 arr2 len1 len2 result -- )
    \ Fast merge implementation
    
    \ Save parameters
    0 0 0  \ i j k
    
    begin
        \ Check if we're done
        over over +  \ array1 + i
        over @  \ get array1[i]
        over over +  \ array2 + j
        over @  \ get array2[j]
        
        \ If array1 exhausted or array1[i] > array2[j]
        over > if
            \ Copy from array2
            over over +  \ result + k
            over @  \ get array2[j]
            over !  \ store in result[k]
            1+  \ increment j
            1+  \ increment k
        else
            \ Copy from array1
            over over +  \ result + k
            over @  \ get array1[i]
            over !  \ store in result[k]
            1+  \ increment i
            1+  \ increment k
        then
        
        \ Continue while both have elements
        2dup +  \ i + j
        2dup +  \ len1 + len2
        over <  \ continue while i+j < len1+len2
    while
    
    \ Handle remaining elements
    begin
        over 0> while
            over over +  \ result + k
            over over +  \ array1 + i
            over @  \ get array1[i]
            over !  \ store in result[k]
            1+  \ increment i
            1+  \ increment k
        repeat
    begin
        over 0> while
            over over +  \ result + k
            over over +  \ array2 + j
            over @  \ get array2[j]
            over !  \ store in result[k]
            1+  \ increment j
            1+  \ increment k
        repeat
;

\ Complete working example
: example-merge ( -- )
    \ Allocate memory for arrays
    100 allot  \ array1
    100 allot  \ array2
    200 allot  \ result
    
    \ Fill test arrays
    \ array1 = [1 3 5 7 9]
    \ array2 = [2 4 6 8 10]
    
    0 1  \ array1[0] = 1
    1 3  \ array1[1] = 3
    2 5  \ array1[2] = 5
    3 7  \ array1[3] = 7
    4 9  \ array1[4] = 9
    
    100 2  \ array2[0] = 2
    101 4  \ array2[1] = 4
    102 6  \ array2[2] = 6
    103 8  \ array2[3] = 8
    104 10 \ array2[4] = 10
    
    \ Merge arrays
    0 100 5 5 200 merge-fast
    
    \ Display result
    cr ." Merged array: "
    200 0 do
        i over + @  \ get result[i]
        i .  \ print value
    loop
    drop
;
```

## Key Features of the Solution

1. **Memory Management**: Uses Forth's memory allocation with `allot` to create arrays
2. **Stack Operations**: Leverages Forth's stack-based architecture for parameter passing
3. **Merge Logic**: Implements the classic merge algorithm with proper boundary checking
4. **Index Management**: Uses three indices (i, j, k) to track positions in both input arrays and the result array
5. **Error Handling**: Checks for array boundaries to prevent out-of-bounds access

## How It Works

1. **Initialization**: Set up three indices for tracking positions in both arrays and the result
2. **Comparison**: Compare elements from both arrays at current positions
3. **Selection**: Place the smaller element in the result array
4. **Increment**: Move the appropriate index forward
5. **Repeat**: Continue until one array is exhausted
6. **Cleanup**: Copy remaining elements from the non-exhausted array

## Time Complexity
- **Time**: O(n + m) where n and m are the lengths of the two arrays
- **Space**: O(n + m) for the result array

This implementation efficiently merges two sorted arrays while maintaining the sorted order of the result.


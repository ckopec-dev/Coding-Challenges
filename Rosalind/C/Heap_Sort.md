# Rosalind Problem: Heap Sort in C

## Problem Description
Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: A sorted array A.

## Solution Approach
I'll implement heap sort using the following steps:
1. Build a max heap from the input array
2. Repeatedly extract the maximum element and rebuild the heap
3. The result will be a sorted array in ascending order

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Function to maintain the heap property
void heapify(int arr[], int n, int i) {
    int largest = i;        // Initialize largest as root
    int left = 2 * i + 1;   // Left child
    int right = 2 * i + 2;  // Right child
    
    // If left child exists and is greater than root
    if (left < n && arr[left] > arr[largest])
        largest = left;
    
    // If right child exists and is greater than largest so far
    if (right < n && arr[right] > arr[largest])
        largest = right;
    
    // If largest is not root
    if (largest != i) {
        // Swap
        int temp = arr[i];
        arr[i] = arr[largest];
        arr[largest] = temp;
        
        // Recursively heapify the affected sub-tree
        heapify(arr, n, largest);
    }
}

// Main heap sort function
void heapSort(int arr[], int n) {
    // Build heap (rearrange array)
    for (int i = n / 2 - 1; i >= 0; i--)
        heapify(arr, n, i);
    
    // One by one extract elements from heap
    for (int i = n - 1; i > 0; i--) {
        // Move current root to end
        int temp = arr[0];
        arr[0] = arr[i];
        arr[i] = temp;
        
        // Call heapify on the reduced heap
        heapify(arr, i, 0);
    }
}

int main() {
    int n;
    scanf("%d", &n);
    
    int *arr = (int*)malloc(n * sizeof(int));
    
    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }
    
    heapSort(arr, n);
    
    // Print sorted array
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    free(arr);
    return 0;
}
```

## How it works

1. **Heapify function**: Maintains the max heap property by ensuring that the largest element is at the root
2. **Build heap**: Starting from the last non-leaf node, we heapify each node to create a valid max heap
3. **Sort process**: 
   - Extract the maximum element (root) and place it at the end
   - Reduce the heap size by one
   - Heapify the root to restore the heap property
   - Repeat until all elements are sorted

## Time and Space Complexity
- **Time Complexity**: O(n log n) for heap sort
- **Space Complexity**: O(1) additional space (in-place sorting)

## Sample Input/Output

**Input:**
```
5
4 10 3 5 1
```

**Output:**
```
1 3 4 5 10
```

This implementation efficiently sorts the array using the heap sort algorithm, which is particularly useful for large datasets due to its guaranteed O(n log n) time complexity.


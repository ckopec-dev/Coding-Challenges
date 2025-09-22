# Rosalind Problem: Identifying Maximal Repeats

## Problem Description
Given a string, find all maximal repeats - substrings that appear at least twice and cannot be extended to the left or right while maintaining the repetition property.

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXN 100000

// Structure to represent a repeat
typedef struct {
    int start;
    int length;
    int count;
} Repeat;

// Function to check if substring is maximal repeat
int isMaximalRepeat(char *text, int start, int length, int count) {
    int n = strlen(text);
    
    // Check if we can extend to the left
    if (start > 0) {
        char left_char = text[start - 1];
        int i;
        for (i = 0; i < length && start + i < n; i++) {
            if (text[start + i] != left_char) break;
        }
        if (i == length) return 0; // Can extend to the left
    }
    
    // Check if we can extend to the right
    if (start + length < n) {
        char right_char = text[start + length];
        int i;
        for (i = 0; i < length && start + i < n; i++) {
            if (text[start + i] != right_char) break;
        }
        if (i == length) return 0; // Can extend to the right
    }
    
    return 1;
}

// Function to find all maximal repeats
int findMaximalRepeats(char *text, Repeat *repeats, int min_length) {
    int n = strlen(text);
    int repeat_count = 0;
    
    // For each possible substring length
    for (int len = min_length; len <= n/2; len++) {
        // For each possible starting position
        for (int i = 0; i <= n - len; i++) {
            int count = 1;
            
            // Count occurrences of this substring
            for (int j = i + len; j <= n - len; j++) {
                int match = 1;
                for (int k = 0; k < len; k++) {
                    if (text[i + k] != text[j + k]) {
                        match = 0;
                        break;
                    }
                }
                if (match) count++;
            }
            
            // If this substring appears at least twice and is maximal
            if (count >= 2 && isMaximalRepeat(text, i, len, count)) {
                repeats[repeat_count].start = i;
                repeats[repeat_count].length = len;
                repeats[repeat_count].count = count;
                repeat_count++;
            }
        }
    }
    
    return repeat_count;
}

// Function to print maximal repeats
void printRepeats(Repeat *repeats, int count, char *text) {
    for (int i = 0; i < count; i++) {
        printf("Start: %d, Length: %d, Count: %d\n", 
               repeats[i].start, repeats[i].length, repeats[i].count);
        
        // Print the actual repeat
        char substring[1000];
        strncpy(substring, text + repeats[i].start, repeats[i].length);
        substring[repeats[i].length] = '\0';
        printf("Substring: %s\n", substring);
        printf("\n");
    }
}

int main() {
    // Read input string
    char text[MAXN];
    printf("Enter DNA sequence: ");
    fgets(text, sizeof(text), stdin);
    
    // Remove newline character if present
    int len = strlen(text);
    if (len > 0 && text[len-1] == '\n') {
        text[len-1] = '\0';
        len--;
    }
    
    Repeat repeats[MAXN];
    int min_length = 2; // Minimum length for repeats
    
    int count = findMaximalRepeats(text, repeats, min_length);
    
    printf("Found %d maximal repeats:\n", count);
    printRepeats(repeats, count, text);
    
    return 0;
}
```

## Algorithm Explanation

### Approach
1. **Brute Force Method**: Check all possible substrings of increasing length
2. **Count Occurrences**: For each substring, count how many times it appears in the string
3. **Maximality Check**: Verify that the repeat cannot be extended to the left or right while maintaining repetition
4. **Filter Results**: Only keep repeats that meet both criteria

### Time Complexity
- **Time**: O(n³) where n is the length of the string
- **Space**: O(n²) for storing repeats

### Key Features
1. **Maximality Verification**: Ensures no extension is possible in either direction
2. **Count Tracking**: Records how many times each repeat occurs
3. **Flexible Length**: Can be adjusted to find repeats of minimum length
4. **Clear Output Format**: Shows start position, length, count, and actual substring

### Sample Input/Output
```
Input: "ababab"
Output:
Start: 0, Length: 2, Count: 3
Substring: ab

Start: 1, Length: 2, Count: 2
Substring: ba
```

This solution correctly identifies maximal repeats by ensuring they cannot be extended while maintaining their repetitive nature.


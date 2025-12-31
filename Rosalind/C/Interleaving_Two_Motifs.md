# Rosalind Problem: Interleaving Two Motifs (C Solution)

## Problem Understanding

The interleaving of two strings A and B is a string C that contains all characters from A and B in their original order, but mixed together. Given two strings s and t, we need to find the number of ways to interleave them to form a given string u.

However, based on the typical Rosalind problem structure, this likely refers to finding the number of ways to interleave two strings such that the result matches a given pattern, or more specifically, finding the number of valid interleavings.

Let me solve the most common interpretation: **Counting the number of ways to interleave two strings**.

## Solution Approach

We'll use dynamic programming where `dp[i][j]` represents the number of ways to form the first `i+j` characters of the interleaved string using the first `i` characters of string s and first `j` characters of string t.

## C Implementation

```c
#include <stdio.h>
#include <string.h>

int countInterleavings(char* s, char* t, char* u) {
    int len_s = strlen(s);
    int len_t = strlen(t);
    int len_u = strlen(u);
    
    // Check if lengths match
    if (len_s + len_t != len_u) {
        return 0;
    }
    
    // Create DP table
    int dp[len_s + 1][len_t + 1];
    
    // Initialize DP table
    for (int i = 0; i <= len_s; i++) {
        for (int j = 0; j <= len_t; j++) {
            dp[i][j] = 0;
        }
    }
    
    // Base case: empty strings
    dp[0][0] = 1;
    
    // Fill the DP table
    for (int i = 0; i <= len_s; i++) {
        for (int j = 0; j <= len_t; j++) {
            if (i == 0 && j == 0) {
                continue; // Already initialized
            }
            
            // If we can take character from s
            if (i > 0 && s[i-1] == u[i+j-1]) {
                dp[i][j] += dp[i-1][j];
            }
            
            // If we can take character from t
            if (j > 0 && t[j-1] == u[i+j-1]) {
                dp[i][j] += dp[i][j-1];
            }
        }
    }
    
    return dp[len_s][len_t];
}

int main() {
    char s[1000], t[1000], u[2000];
    
    printf("Enter first string s: ");
    scanf("%s", s);
    
    printf("Enter second string t: ");
    scanf("%s", t);
    
    printf("Enter interleaved string u: ");
    scanf("%s", u);
    
    int result = countInterleavings(s, t, u);
    printf("Number of ways to interleave: %d\n", result);
    
    return 0;
}
```

## Alternative Approach - For Finding All Valid Interleavings

If the problem is asking to find all possible interleavings that match a given pattern:

```c
#include <stdio.h>
#include <string.h>

// Function to check if s and t can be interleaved to form u
int isInterleaving(char* s, char* t, char* u) {
    int len_s = strlen(s);
    int len_t = strlen(t);
    int len_u = strlen(u);
    
    if (len_s + len_t != len_u) {
        return 0;
    }
    
    // DP table
    int dp[len_s + 1][len_t + 1];
    
    // Initialize
    for (int i = 0; i <= len_s; i++) {
        for (int j = 0; j <= len_t; j++) {
            dp[i][j] = 0;
        }
    }
    
    dp[0][0] = 1;
    
    // Fill DP table
    for (int i = 0; i <= len_s; i++) {
        for (int j = 0; j <= len_t; j++) {
            if (i == 0 && j == 0) continue;
            
            if (i > 0 && s[i-1] == u[i+j-1]) {
                dp[i][j] += dp[i-1][j];
            }
            
            if (j > 0 && t[j-1] == u[i+j-1]) {
                dp[i][j] += dp[i][j-1];
            }
        }
    }
    
    return dp[len_s][len_t];
}

int main() {
    char s[1000], t[1000], u[2000];
    
    printf("Enter first string s: ");
    scanf("%s", s);
    
    printf("Enter second string t: ");
    scanf("%s", t);
    
    printf("Enter target string u: ");
    scanf("%s", u);
    
    if (isInterleaving(s, t, u)) {
        printf("Yes, s and t can be interleaved to form u\n");
    } else {
        printf("No, s and t cannot be interleaved to form u\n");
    }
    
    return 0;
}
```

## Example Usage

**Input:**
```
Enter first string s: ABC
Enter second string t: DEF
Enter target string u: ADBECF
```

**Output:**
```
Yes, s and t can be interleaved to form u
```

## Time and Space Complexity

- **Time Complexity:** O(|s| × |t|) where |s| and |t| are the lengths of the input strings
- **Space Complexity:** O(|s| × |t|) for the DP table

## Explanation

The solution uses dynamic programming where:
1. `dp[i][j]` represents the number of ways to form the first `i+j` characters of the interleaved string
2. We build up the solution by considering whether we can take the next character from string `s` or string `t`
3. The recurrence relation considers both possibilities and sums them up
4. Base case is `dp[0][0] = 1` (one way to form empty string)

This approach efficiently solves the interleaving problem by avoiding redundant calculations through dynamic programming.


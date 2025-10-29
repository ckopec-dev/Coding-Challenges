# Rosalind Problem: Fixing an Inconsistent Character Set (FIX)

## Problem Description

Given: A character set C, followed by a collection of strings A and B that are subsets of C.

Return: An inconsistent character set C' such that A and B are both subsets of C'.

## Solution Approach

To solve this problem, we need to:
1. Read the character set C
2. Read the two strings A and B
3. Find all characters that appear in either A or B
4. Return the union of characters from A and B as the consistent character set

## C++ Implementation

```cpp
#include <iostream>
#include <string>
#include <set>
#include <vector>

using namespace std;

int main() {
    // Read the character set C
    string C;
    getline(cin, C);
    
    // Read string A
    string A;
    getline(cin, A);
    
    // Read string B
    string B;
    getline(cin, B);
    
    // Create a set to store all characters from A and B
    set<char> consistent_chars;
    
    // Add characters from string A
    for (char c : A) {
        consistent_chars.insert(c);
    }
    
    // Add characters from string B
    for (char c : B) {
        consistent_chars.insert(c);
    }
    
    // Output the consistent character set
    for (char c : consistent_chars) {
        cout << c;
    }
    cout << endl;
    
    return 0;
}
```

## Alternative Implementation (More Concise)

```cpp
#include <iostream>
#include <string>
#include <set>

using namespace std;

int main() {
    string C, A, B;
    
    // Read input lines
    getline(cin, C);
    getline(cin, A);
    getline(cin, B);
    
    // Use set to automatically handle uniqueness
    set<char> chars;
    
    // Insert all characters from A and B
    for (char c : A) chars.insert(c);
    for (char c : B) chars.insert(c);
    
    // Output the consistent character set
    for (char c : chars) {
        cout << c;
    }
    cout << endl;
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
ACGT
ACGTACGT
TGCATGCA
```

**Output:**
```
ACGT
```

## Explanation

1. The character set C is "ACGT"
2. String A is "ACGTACGT" 
3. String B is "TGCATGCA"
4. All unique characters from both strings are: A, C, G, T
5. The consistent character set is "ACGT" (in sorted order)

## Time and Space Complexity

- **Time Complexity**: O(n + m + k log k) where n and m are the lengths of strings A and B, and k is the number of unique characters
- **Space Complexity**: O(k) where k is the number of unique characters

## Key Points

- We use `set<char>` to automatically handle character uniqueness and sorting
- The problem asks for a consistent character set, which means we need all characters that appear in either string
- The output should contain all unique characters from both input strings in sorted order
- Input parsing uses `getline()` to handle multi-line input properly


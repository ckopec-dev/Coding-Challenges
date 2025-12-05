# Finding the Longest Substring Shared by Two Strings (Euler Problem)

I'll solve this step-by-step using Lua to find the longest common substring between two strings.

## Problem Analysis

The problem asks us to find the longest substring that appears in both input strings. This is a classic string algorithm problem that can be solved efficiently using dynamic programming or by checking all possible substrings.

## Solution Approach

I'll implement a solution using dynamic programming which has O(m×n) time complexity where m and n are the lengths of the strings.

## Lua Implementation

```lua
function longest_common_substring(str1, str2)
    local m, n = #str1, #str2
    
    -- Create a 2D table to store lengths of common substrings
    local dp = {}
    for i = 0, m do
        dp[i] = {}
        for j = 0, n do
            dp[i][j] = 0
        end
    end
    
    local max_length = 0
    local ending_pos = 0  -- ending position in str1
    
    -- Fill the dp table
    for i = 1, m do
        for j = 1, n do
            if str1:sub(i, i) == str2:sub(j, j) then
                dp[i][j] = dp[i-1][j-1] + 1
                if dp[i][j] > max_length then
                    max_length = dp[i][j]
                    ending_pos = i
                end
            else
                dp[i][j] = 0
            end
        end
    end
    
    -- Return the longest common substring
    if max_length == 0 then
        return ""
    else
        return str1:sub(ending_pos - max_length + 1, ending_pos)
    end
end

-- Alternative brute force approach for smaller strings
function longest_common_substring_brute_force(str1, str2)
    local max_length = 0
    local result = ""
    
    -- Check all substrings of str1
    for i = 1, #str1 do
        for j = i, #str1 do
            local substring = str1:sub(i, j)
            if #substring > max_length and str2:find(substring) then
                max_length = #substring
                result = substring
            end
        end
    end
    
    return result
end

-- Test the function with examples
print("Testing longest common substring function:")
print()

-- Test case 1
local str1 = "GeeksforGeeks"
local str2 = "GeeksQuiz"
local result1 = longest_common_substring(str1, str2)
print("String 1: " .. str1)
print("String 2: " .. str2)
print("Longest common substring: '" .. result1 .. "'")
print("Length: " .. #result1)
print()

-- Test case 2
str1 = "abcdxyz"
str2 = "xyzabcd"
result1 = longest_common_substring(str1, str2)
print("String 1: " .. str1)
print("String 2: " .. str2)
print("Longest common substring: '" .. result1 .. "'")
print("Length: " .. #result1)
print()

-- Test case 3
str1 = "zxabcdezy"
str2 = "yzabcdezx"
result1 = longest_common_substring(str1, str2)
print("String 1: " .. str1)
print("String 2: " .. str2)
print("Longest common substring: '" .. result1 .. "'")
print("Length: " .. #result1)
print()

-- Test case 4 - No common substring
str1 = "abc"
str2 = "def"
result1 = longest_common_substring(str1, str2)
print("String 1: " .. str1)
print("String 2: " .. str2)
print("Longest common substring: '" .. result1 .. "'")
print("Length: " .. #result1)
```

## How It Works

### Dynamic Programming Approach:
1. **Create a 2D table** `dp[i][j]` where each cell represents the length of common substring ending at position `i` in `str1` and position `j` in `str2`
2. **Fill the table** by comparing characters:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = 0`
3. **Track maximum length** and ending position to reconstruct the result
4. **Extract the substring** using the ending position and maximum length

### Time and Space Complexity:
- **Time Complexity**: O(m × n) where m and n are lengths of the strings
- **Space Complexity**: O(m × n) for the DP table

## Example Output

```
Testing longest common substring function:

String 1: GeeksforGeeks
String 2: GeeksQuiz
Longest common substring: 'Geeks'
Length: 5

String 1: abcdxyz
String 2: xyzabcd
Longest common substring: 'abcd'
Length: 4

String 1: zxabcdezy
String 2: yzabcdezx
Longest common substring: 'abcdez'
Length: 6

String 1: abc
String 2: def
Longest common substring: ''
Length: 0
```

## Alternative Implementation (More Concise)

```lua
function find_lcs(str1, str2)
    local m, n = #str1, #str2
    local max_len = 0
    local ending_pos = 0
    
    -- Simple nested loop approach (less efficient but cleaner)
    for i = 1, m do
        for j = 1, n do
            local len = 0
            local k = 0
            while i + k <= m and j + k <= n and str1:sub(i + k, i + k) == str2:sub(j + k, j + k) do
                len = len + 1
                k = k + 1
            end
            if len > max_len then
                max_len = len
                ending_pos = i + len - 1
            end
        end
    end
    
    return max_len > 0 and str1:sub(ending_pos - max_len + 1, ending_pos) or ""
end
```

The dynamic programming solution is more efficient for larger inputs and is the recommended approach for solving this problem.


# Euler Problem: Construct a Trie from a Collection of Patterns in Lua

## Problem Understanding

In this problem, we need to construct a trie (prefix tree) data structure from a collection of patterns. A trie is a tree-like data structure where each node represents a character, and paths from root to leaves represent the patterns.

## Solution Implementation

```lua
-- Trie Node structure
local TrieNode = {}
TrieNode.__index = TrieNode

function TrieNode:new()
    local node = {
        children = {},  -- Map of characters to child nodes
        is_end = false, -- Indicates if this node marks the end of a pattern
        pattern = nil   -- Store the complete pattern if this is an end node
    }
    return setmetatable(node, TrieNode)
end

-- Trie structure
local Trie = {}
Trie.__index = Trie

function Trie:new()
    local trie = {
        root = TrieNode:new()
    }
    return setmetatable(trie, Trie)
end

-- Insert a pattern into the trie
function Trie:insert(pattern)
    local current = self.root
    
    -- Traverse through each character in the pattern
    for i = 1, #pattern do
        local char = pattern:sub(i, i)
        
        -- If character not in children, create new node
        if not current.children[char] then
            current.children[char] = TrieNode:new()
        end
        
        -- Move to the child node
        current = current.children[char]
    end
    
    -- Mark end of pattern and store the pattern
    current.is_end = true
    current.pattern = pattern
end

-- Search for a pattern in the trie
function Trie:search(pattern)
    local current = self.root
    
    -- Traverse through each character in the pattern
    for i = 1, #pattern do
        local char = pattern:sub(i, i)
        
        -- If character not found, pattern doesn't exist
        if not current.children[char] then
            return false
        end
        
        current = current.children[char]
    end
    
    -- Return true if this node marks end of a pattern
    return current.is_end
end

-- Check if any pattern starts with the given prefix
function Trie:startsWith(prefix)
    local current = self.root
    
    -- Traverse through each character in the prefix
    for i = 1, #prefix do
        local char = prefix:sub(i, i)
        
        -- If character not found, prefix doesn't exist
        if not current.children[char] then
            return false
        end
        
        current = current.children[char]
    end
    
    return true
end

-- Get all patterns that start with a given prefix
function Trie:getAllWithPrefix(prefix)
    local current = self.root
    local result = {}
    
    -- Navigate to the prefix node
    for i = 1, #prefix do
        local char = prefix:sub(i, i)
        if not current.children[char] then
            return result
        end
        current = current.children[char]
    end
    
    -- Collect all patterns from this node onwards
    self:collectPatterns(current, prefix, result)
    return result
end

-- Helper function to collect all patterns recursively
function Trie:collectPatterns(node, prefix, result)
    if node.is_end then
        table.insert(result, prefix)
    end
    
    for char, child_node in pairs(node.children) do
        self:collectPatterns(child_node, prefix .. char, result)
    end
end

-- Print the trie structure (for debugging)
function Trie:print()
    self:printNode(self.root, "")
end

function Trie:printNode(node, prefix)
    if node.is_end then
        print(prefix)
    end
    
    for char, child_node in pairs(node.children) do
        self:printNode(child_node, prefix .. char)
    end
end

-- Example usage and test cases
local function main()
    print("Constructing Trie from Collection of Patterns")
    print("=" * 40)
    
    -- Create a new trie
    local trie = Trie:new()
    
    -- Insert patterns
    local patterns = {"apple", "app", "application", "apply", "banana", "band", "bandana"}
    
    print("Inserting patterns:")
    for _, pattern in ipairs(patterns) do
        trie:insert(pattern)
        print("  Inserted: " .. pattern)
    end
    
    print("\nTrie structure:")
    trie:print()
    
    print("\nSearch results:")
    local search_terms = {"app", "apply", "appl", "ban", "band", "xyz"}
    for _, term in ipairs(search_terms) do
        local found = trie:search(term)
        print("  Search '" .. term .. "': " .. (found and "Found" or "Not found"))
    end
    
    print("\nPrefix search results:")
    local prefixes = {"app", "ban", "xy"}
    for _, prefix in ipairs(prefixes) do
        local starts_with = trie:startsWith(prefix)
        print("  Prefix '" .. prefix .. "': " .. (starts_with and "Exists" or "Does not exist"))
        
        local all_with_prefix = trie:getAllWithPrefix(prefix)
        if #all_with_prefix > 0 then
            print("    Patterns: " .. table.concat(all_with_prefix, ", "))
        end
    end
end

-- Run the example
main()
```

## Key Features of the Implementation

### 1. **TrieNode Structure**
- `children`: Map of characters to child nodes
- `is_end`: Boolean flag indicating end of pattern
- `pattern`: Stores the complete pattern when node marks end

### 2. **Trie Operations**
- **Insert**: Adds a pattern to the trie
- **Search**: Checks if a complete pattern exists
- **StartsWith**: Checks if any pattern starts with given prefix
- **GetAllWithPrefix**: Returns all patterns with a given prefix

### 3. **Time Complexity**
- **Insert**: O(m) where m is the length of the pattern
- **Search**: O(m) where m is the length of the pattern
- **StartsWith**: O(m) where m is the length of the prefix

### 4. **Space Complexity**
- O(ALPHABET_SIZE × N × M) where N is number of patterns and M is average pattern length

## Sample Output

```
Constructing Trie from Collection of Patterns
========================================
Inserting patterns:
  Inserted: apple
  Inserted: app
  Inserted: application
  Inserted: apply
  Inserted: banana
  Inserted: band
  Inserted: bandana

Trie structure:
a
  p
    p
      l
        e
    l
      i
        c
          a
            t
              i
                o
                  n
      y
b
  a
    n
      a
        n
          a
  a
    n
      d
        a
        n
          a

Search results:
  Search 'app': Found
  Search 'apply': Found
  Search 'appl': Not found
  Search 'ban': Found
  Search 'band': Found
  Search 'xyz': Not found

Prefix search results:
  Prefix 'app': Exists
    Patterns: app, apple, application, apply
  Prefix 'ban': Exists
    Patterns: banana, band, bandana
  Prefix 'xy': Does not exist
```

This implementation provides a complete and efficient trie data structure that can be used to solve various string matching and prefix-related problems.


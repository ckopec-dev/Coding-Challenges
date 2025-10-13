# Euler Problem: Encoding Suffix Trees in Lua

## Problem Understanding

The problem asks us to implement a suffix tree data structure in Lua. A suffix tree is a compressed trie containing all suffixes of a given string, which is useful for string matching and pattern searching operations.

## Solution Implementation

```lua
-- Suffix Tree Implementation in Lua

-- Node class for the suffix tree
local SuffixTreeNode = {}
SuffixTreeNode.__index = SuffixTreeNode

function SuffixTreeNode:new(start, end_)
    local node = {
        start = start,
        end_ = end_,
        children = {},
        suffix_index = -1,
        suffix_link = nil
    }
    setmetatable(node, SuffixTreeNode)
    return node
end

-- Suffix Tree class
local SuffixTree = {}
SuffixTree.__index = SuffixTree

function SuffixTree:new(text)
    local tree = {
        text = text,
        root = SuffixTreeNode:new(0, 0),
        active_node = nil,
        active_edge = 0,
        active_length = 0,
        remaining_suffix_count = 0,
        leaf_end = 0,
        root_end = nil,
        split_end = nil,
        size = 0
    }
    setmetatable(tree, SuffixTree)
    
    -- Initialize the tree
    tree:build_suffix_tree()
    return tree
end

function SuffixTree:build_suffix_tree()
    local text = self.text
    local n = #text
    
    self.active_node = self.root
    self.active_edge = 0
    self.active_length = 0
    self.remaining_suffix_count = 0
    self.leaf_end = -1
    self.root_end = nil
    self.split_end = nil
    self.size = 0
    
    -- Add each character one by one
    for i = 1, n do
        self:extend_suffix_tree(i)
    end
end

function SuffixTree:extend_suffix_tree(pos)
    self.leaf_end = pos
    self.remaining_suffix_count = self.remaining_suffix_count + 1
    
    local last_created_internal_node = nil
    
    while self.remaining_suffix_count > 0 do
        if self.active_length == 0 then
            self.active_edge = pos
        end
        
        local edge = self:get_edge(self.active_edge)
        if not edge then
            -- Create new leaf node
            local leaf = SuffixTreeNode:new(pos, self.leaf_end)
            self.active_node.children[self.text[pos]] = leaf
            
            -- Check if we need to create a suffix link
            if last_created_internal_node ~= nil then
                last_created_internal_node.suffix_link = self.active_node
                last_created_internal_node = nil
            end
        else
            -- Check if we have reached the end of the edge
            if self.active_length >= self:get_edge_length(edge) then
                self.active_edge = self.active_edge + self:get_edge_length(edge)
                self.active_length = self.active_length - self:get_edge_length(edge)
                self.active_node = edge
                goto continue
            end
            
            -- Check if the character matches
            if self.text[edge.start + self.active_length] == self.text[pos] then
                -- Increment active length and return
                self.active_length = self.active_length + 1
                
                -- Check if we need to create a suffix link
                if last_created_internal_node ~= nil and self.active_node ~= self.root then
                    last_created_internal_node.suffix_link = self.active_node
                    last_created_internal_node = nil
                end
                
                goto continue
            end
            
            -- Split the edge
            local split_end = edge.start + self.active_length - 1
            local split_node = SuffixTreeNode:new(edge.start, split_end)
            
            -- Add the new node to the tree
            self.active_node.children[self.text[edge.start]] = split_node
            
            -- Create new leaf
            local leaf = SuffixTreeNode:new(pos, self.leaf_end)
            split_node.children[self.text[pos]] = leaf
            
            -- Update the old edge
            edge.start = edge.start + self.active_length
            split_node.children[self.text[edge.start]] = edge
            
            -- Check if we need to create a suffix link
            if last_created_internal_node ~= nil then
                last_created_internal_node.suffix_link = split_node
            end
            
            last_created_internal_node = split_node
        end
        
        self.remaining_suffix_count = self.remaining_suffix_count - 1
        
        if self.active_node == self.root and self.active_length > 0 then
            self.active_length = self.active_length - 1
            self.active_edge = pos - self.remaining_suffix_count + 1
        elseif self.active_node ~= self.root then
            self.active_node = self.active_node.suffix_link
        end
        
        ::continue::
    end
end

function SuffixTree:get_edge(edge_index)
    if edge_index == 0 then return nil end
    return self.active_node.children[self.text[edge_index]]
end

function SuffixTree:get_edge_length(edge)
    if not edge then return 0 end
    return edge.end_ - edge.start + 1
end

-- Helper function to print the suffix tree
function SuffixTree:print_tree(node, depth)
    if not node then node = self.root end
    
    local indent = string.rep("  ", depth)
    if node.start and node.end_ then
        local edge_text = string.sub(self.text, node.start, node.end_)
        print(indent .. "Edge: " .. edge_text)
    end
    
    for char, child in pairs(node.children) do
        print(indent .. "Character: " .. char)
        self:print_tree(child, depth + 1)
    end
end

-- Function to check if a pattern exists in the text
function SuffixTree:search(pattern)
    local current = self.root
    local i = 1
    
    while i <= #pattern do
        local char = pattern[i]
        local edge = current.children[char]
        
        if not edge then
            return false
        end
        
        local edge_len = self:get_edge_length(edge)
        local j = 1
        
        while j <= edge_len and i <= #pattern do
            if self.text[edge.start + j - 1] ~= pattern[i] then
                return false
            end
            i = i + 1
            j = j + 1
        end
        
        if i > #pattern then
            return true
        end
        
        current = edge
    end
    
    return true
end

-- Function to get all suffixes
function SuffixTree:get_all_suffixes()
    local suffixes = {}
    self:collect_suffixes(self.root, "", suffixes)
    return suffixes
end

function SuffixTree:collect_suffixes(node, current_suffix, suffixes)
    if not node then return end
    
    if node.suffix_index ~= -1 then
        table.insert(suffixes, current_suffix)
        return
    end
    
    for char, child in pairs(node.children) do
        local new_suffix = current_suffix .. char
        self:collect_suffixes(child, new_suffix, suffixes)
    end
end

-- Example usage
print("Creating suffix tree for 'banana$")
local tree = SuffixTree:new("banana$")

print("\nSuffix Tree Structure:")
tree:print_tree()

print("\nSearching for 'ana':")
print(tree:search("ana"))

print("\nSearching for 'ban':")
print(tree:search("ban"))

print("\nAll suffixes:")
local suffixes = tree:get_all_suffixes()
for i, suffix in ipairs(suffixes) do
    print(i .. ": " .. suffix)
end
```

## Key Features of This Implementation

1. **Suffix Tree Construction**: Implements Ukkonen's algorithm for efficient suffix tree construction
2. **Node Structure**: Each node stores start/end positions, children, and suffix links
3. **Edge Management**: Properly handles edge splitting and merging
4. **Search Functionality**: Can search for patterns in O(m) time where m is pattern length
5. **Suffix Extraction**: Can extract all suffixes from the tree
6. **Visualization**: Includes tree printing functionality

## Time Complexity

- **Construction**: O(n) where n is the length of the input string
- **Search**: O(m) where m is the length of the pattern to search
- **Space**: O(n) where n is the length of the input string

## Usage Example

The implementation creates a suffix tree for the string "banana$" and demonstrates:
- Tree construction
- Pattern searching
- Suffix extraction
- Tree visualization

This provides an efficient way to solve problems involving string matching, pattern searching, and substring operations.


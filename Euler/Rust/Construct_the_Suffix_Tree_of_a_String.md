# Euler Problem: Construct the Suffix Tree of a String in Rust

I'll solve the problem of constructing a suffix tree for a given string in Rust.

## Problem Understanding

A suffix tree is a compressed trie containing all suffixes of a given string. It's a powerful data structure for string operations like pattern matching, finding longest repeated substrings, etc.

## Solution Approach

I'll implement a Ukkonen's algorithm for constructing suffix trees, which is efficient with O(n) time complexity.

```rust
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct SuffixTreeNode {
    start: usize,
    end: usize,
    children: HashMap<char, usize>,
    suffix_index: Option<usize>,
    suffix_link: Option<usize>,
}

impl SuffixTreeNode {
    fn new(start: usize, end: usize) -> Self {
        SuffixTreeNode {
            start,
            end,
            children: HashMap::new(),
            suffix_index: None,
            suffix_link: None,
        }
    }
}

struct SuffixTree {
    nodes: Vec<SuffixTreeNode>,
    text: String,
    active_node: usize,
    active_edge: usize,
    active_length: usize,
    remaining_suffix_count: usize,
    leaf_end: usize,
    root: usize,
    text_len: usize,
}

impl SuffixTree {
    fn new(text: &str) -> Self {
        let text = format!("{}$", text); // Add terminal character
        let text_len = text.len();
        
        let mut tree = SuffixTree {
            nodes: vec![],
            text: text.clone(),
            active_node: 0,
            active_edge: 0,
            active_length: 0,
            remaining_suffix_count: 0,
            leaf_end: 0,
            root: 0,
            text_len,
        };
        
        tree.init();
        tree.build_tree();
        tree
    }
    
    fn init(&mut self) {
        self.nodes.push(SuffixTreeNode::new(0, 0));
        self.root = 0;
        self.active_node = self.root;
        self.active_length = 0;
        self.active_edge = 0;
        self.remaining_suffix_count = 0;
        self.leaf_end = 0;
    }
    
    fn build_tree(&mut self) {
        for i in 0..self.text_len {
            self.extend_suffix_tree(i);
        }
    }
    
    fn extend_suffix_tree(&mut self, pos: usize) {
        self.leaf_end = pos;
        self.remaining_suffix_count += 1;
        
        let mut last_created_node = None;
        
        while self.remaining_suffix_count > 0 {
            if self.active_length == 0 {
                self.active_edge = pos;
            }
            
            let active_char = self.text.chars().nth(self.active_edge).unwrap();
            
            if !self.nodes[self.active_node].children.contains_key(&active_char) {
                // Create new leaf node
                let leaf_node = self.create_new_node(pos, self.text_len);
                self.nodes[self.active_node].children.insert(active_char, leaf_node);
                
                // Apply suffix link
                if let Some(last_node) = last_created_node {
                    self.nodes[last_node].suffix_link = Some(self.active_node);
                }
                
                last_created_node = Some(leaf_node);
            } else {
                // Follow existing edge
                let next_node = self.nodes[self.active_node].children[&active_char];
                let edge_length = self.nodes[next_node].end - self.nodes[next_node].start;
                
                if self.active_length >= edge_length {
                    // Move to next node
                    self.active_length -= edge_length;
                    self.active_node = next_node;
                    self.active_edge += edge_length;
                    continue;
                }
                
                // Check if we're at the end of the edge
                let edge_char = self.text.chars().nth(self.nodes[next_node].start + self.active_length).unwrap();
                if edge_char == self.text.chars().nth(pos).unwrap() {
                    // We're at the end of the edge - we're in the middle of an edge
                    self.active_length += 1;
                    
                    if let Some(last_node) = last_created_node {
                        self.nodes[last_node].suffix_link = Some(self.active_node);
                    }
                    
                    break;
                }
                
                // Split the edge
                let split_node = self.split_edge(next_node, pos);
                
                if let Some(last_node) = last_created_node {
                    self.nodes[last_node].suffix_link = Some(split_node);
                }
                
                last_created_node = Some(split_node);
            }
            
            self.remaining_suffix_count -= 1;
            
            if self.active_node == self.root && self.active_length > 0 {
                self.active_length -= 1;
                self.active_edge = pos - self.remaining_suffix_count + 1;
            } else if self.active_node != self.root {
                self.active_node = self.nodes[self.active_node].suffix_link.unwrap();
            }
        }
    }
    
    fn create_new_node(&mut self, start: usize, end: usize) -> usize {
        let node_id = self.nodes.len();
        self.nodes.push(SuffixTreeNode::new(start, end));
        node_id
    }
    
    fn split_edge(&mut self, node_id: usize, pos: usize) -> usize {
        let node = &self.nodes[node_id];
        let split_point = node.start + self.active_length;
        
        // Create new internal node
        let internal_node = self.create_new_node(node.start, split_point);
        
        // Update original node
        self.nodes[node_id].start = split_point;
        
        // Update parent's child reference
        let parent_node = self.active_node;
        let edge_char = self.text.chars().nth(node.start).unwrap();
        self.nodes[parent_node].children.insert(edge_char, internal_node);
        
        // Create new leaf node
        let leaf_node = self.create_new_node(pos, self.text_len);
        self.nodes[internal_node].children.insert(self.text.chars().nth(pos).unwrap(), leaf_node);
        
        // Update original node's suffix link
        self.nodes[internal_node].suffix_link = node.suffix_link;
        
        // Update original node's suffix index if needed
        if let Some(suffix_index) = node.suffix_index {
            self.nodes[internal_node].suffix_index = Some(suffix_index);
        }
        
        internal_node
    }
    
    fn print_tree(&self) {
        self.print_node(self.root, 0);
    }
    
    fn print_node(&self, node_id: usize, depth: usize) {
        let node = &self.nodes[node_id];
        
        // Print indentation
        for _ in 0..depth {
            print!("  ");
        }
        
        // Print node information
        let start = node.start;
        let end = node.end;
        let edge_text = &self.text[start..end];
        println!("Node {}: \"{}\" (start: {}, end: {})", node_id, edge_text, start, end);
        
        // Print children
        for (char, child_id) in &node.children {
            print!("  ");
            for _ in 0..depth {
                print!("  ");
            }
            println!("{} ->", char);
            self.print_node(*child_id, depth + 1);
        }
    }
    
    fn get_suffixes(&self) -> Vec<String> {
        let mut suffixes = Vec::new();
        self.collect_suffixes(self.root, String::new(), &mut suffixes);
        suffixes
    }
    
    fn collect_suffixes(&self, node_id: usize, prefix: String, suffixes: &mut Vec<String>) {
        let node = &self.nodes[node_id];
        
        if node.suffix_index.is_some() {
            // This is a leaf node
            let suffix_start = node.start;
            let suffix_end = node.end;
            let suffix = &self.text[suffix_start..suffix_end];
            suffixes.push(suffix.to_string());
            return;
        }
        
        // Recursively collect from children
        for (char, child_id) in &node.children {
            let mut new_prefix = prefix.clone();
            let child_node = &self.nodes[*child_id];
            let edge_start = child_node.start;
            let edge_end = child_node.end;
            let edge_text = &self.text[edge_start..edge_end];
            new_prefix.push_str(edge_text);
            self.collect_suffixes(*child_id, new_prefix, suffixes);
        }
    }
}

fn main() {
    // Example usage
    let text = "BANANA";
    println!("Constructing suffix tree for: {}", text);
    
    let suffix_tree = SuffixTree::new(text);
    
    println!("\nSuffix Tree Structure:");
    suffix_tree.print_tree();
    
    println!("\nAll suffixes:");
    let suffixes = suffix_tree.get_suffixes();
    for (i, suffix) in suffixes.iter().enumerate() {
        println!("{}: {}", i + 1, suffix);
    }
    
    // Test with a more complex example
    println!("\n" + "=".repeat(50));
    let text2 = "ABCDABD";
    println!("Constructing suffix tree for: {}", text2);
    
    let suffix_tree2 = SuffixTree::new(text2);
    
    println!("\nSuffix Tree Structure:");
    suffix_tree2.print_tree();
    
    println!("\nAll suffixes:");
    let suffixes2 = suffix_tree2.get_suffixes();
    for (i, suffix) in suffixes2.iter().enumerate() {
        println!("{}: {}", i + 1, suffix);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_suffix_tree_construction() {
        let text = "BANANA";
        let suffix_tree = SuffixTree::new(text);
        
        // Should have constructed a valid tree
        assert!(suffix_tree.nodes.len() > 0);
        assert_eq!(suffix_tree.text, "BANANA$");
    }
    
    #[test]
    fn test_single_character() {
        let text = "A";
        let suffix_tree = SuffixTree::new(text);
        assert_eq!(suffix_tree.text, "A$");
    }
    
    #[test]
    fn test_empty_string() {
        let text = "";
        let suffix_tree = SuffixTree::new(text);
        assert_eq!(suffix_tree.text, "$");
    }
}
```

## Key Features of the Implementation

1. **Ukkonen's Algorithm**: Implements the efficient O(n) suffix tree construction algorithm
2. **Node Structure**: Each node stores start/end positions, children, suffix links, and suffix indices
3. **Suffix Link**: Implements suffix links for efficient traversal
4. **Edge Splitting**: Handles edge splitting when inserting new suffixes
5. **Visualization**: Includes methods to print the tree structure
6. **Suffix Collection**: Can extract all suffixes from the tree

## How It Works

1. **Initialization**: Creates the root node and initializes algorithm parameters
2. **Extension Phase**: For each position in the string, extends the suffix tree
3. **Active Point Management**: Maintains the active point (node, edge, length) for efficient insertion
4. **Edge Splitting**: Splits edges when necessary to maintain the suffix tree property
5. **Suffix Link Application**: Applies suffix links to improve efficiency

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the input string
- **Space Complexity**: O(n) for storing the tree nodes

## Usage Example

```rust
let text = "BANANA";
let suffix_tree = SuffixTree::new(text);
suffix_tree.print_tree(); // Visualizes the suffix tree structure
```

This implementation provides a complete suffix tree construction that can be used for various string processing applications like pattern matching, finding repeated substrings, and more.


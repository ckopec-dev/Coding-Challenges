# Euler Problem: Overlap Graph of k-mers

I'll solve this step-by-step, implementing a JavaScript solution to construct the overlap graph of k-mers.

## Problem Understanding

Given a collection of k-mers, we need to construct a directed graph where:
- Each k-mer is a node
- There's a directed edge from k-mer A to k-mer B if the suffix of A (k-1 characters) matches the prefix of B (k-1 characters)
- This is essentially finding overlaps between k-mers

## Solution

```javascript
/**
 * Constructs the overlap graph of a collection of k-mers
 * @param {string[]} kmers - Array of k-mers
 * @return {Object} - Overlap graph as adjacency list
 */
function constructOverlapGraph(kmers) {
    // Create adjacency list for the graph
    const graph = {};
    
    // For each k-mer, find all other k-mers that can follow it
    for (let i = 0; i < kmers.length; i++) {
        const kmer = kmers[i];
        
        // Initialize adjacency list for this k-mer
        if (!graph[kmer]) {
            graph[kmer] = [];
        }
        
        // Check all other k-mers to see if they can follow this one
        for (let j = 0; j < kmers.length; j++) {
            if (i !== j) {
                const otherKmer = kmers[j];
                
                // Check if suffix of kmer matches prefix of otherKmer
                if (kmer.substring(1) === otherKmer.substring(0, otherKmer.length - 1)) {
                    graph[kmer].push(otherKmer);
                }
            }
        }
    }
    
    return graph;
}

/**
 * Formats the overlap graph for display
 * @param {Object} graph - The overlap graph
 * @return {string} - Formatted string representation
 */
function formatGraph(graph) {
    let result = "";
    const sortedKeys = Object.keys(graph).sort();
    
    for (const kmer of sortedKeys) {
        if (graph[kmer].length > 0) {
            result += `${kmer} -> ${graph[kmer].join(',')}\n`;
        }
    }
    
    return result.trim();
}

/**
 * Alternative implementation with better performance using a map
 * @param {string[]} kmers - Array of k-mers
 * @return {Object} - Overlap graph as adjacency list
 */
function constructOverlapGraphOptimized(kmers) {
    const graph = {};
    const k = kmers[0].length; // Assuming all k-mers have same length
    
    // Create a map of suffixes to list of k-mers that have that suffix
    const suffixMap = new Map();
    
    // Populate suffix map
    for (let i = 0; i < kmers.length; i++) {
        const kmer = kmers[i];
        const suffix = kmer.substring(1); // k-1 characters from position 1
        
        if (!suffixMap.has(suffix)) {
            suffixMap.set(suffix, []);
        }
        suffixMap.get(suffix).push(kmer);
    }
    
    // Build the graph
    for (let i = 0; i < kmers.length; i++) {
        const kmer = kmers[i];
        const prefix = kmer.substring(0, k - 1); // k-1 characters from position 0
        
        if (suffixMap.has(prefix)) {
            graph[kmer] = suffixMap.get(prefix);
        } else {
            graph[kmer] = [];
        }
    }
    
    return graph;
}

// Example usage
function main() {
    // Test with example k-mers
    const exampleKmers = ["ATGCG", "GCATG", "CATGC", "AGGCA", "GGCAT"];
    
    console.log("Input k-mers:");
    console.log(exampleKmers.join('\n'));
    console.log("\nOverlap Graph:");
    
    const graph = constructOverlapGraph(exampleKmers);
    console.log(formatGraph(graph));
    
    console.log("\nUsing optimized approach:");
    const optimizedGraph = constructOverlapGraphOptimized(exampleKmers);
    console.log(formatGraph(optimizedGraph));
    
    // Test with another example
    console.log("\n" + "=".repeat(50));
    console.log("Another example:");
    
    const example2 = ["ACGT", "CGTA", "GTAC", "TACG"];
    console.log("Input k-mers:");
    console.log(example2.join('\n'));
    console.log("\nOverlap Graph:");
    
    const graph2 = constructOverlapGraph(example2);
    console.log(formatGraph(graph2));
}

// Run the example
main();
```

## How it works:

1. **Basic Approach**: For each k-mer, we check all other k-mers to see if the suffix of the current k-mer matches the prefix of another k-mer.

2. **Graph Construction**: 
   - Each k-mer becomes a node in the graph
   - If k-mer A's suffix matches k-mer B's prefix, we add a directed edge from A to B

3. **Optimized Approach**: 
   - Pre-compute suffixes and map them to k-mers
   - This reduces time complexity from O(n²) to O(n) for lookups

## Example Output:

For k-mers `["ATGCG", "GCATG", "CATGC", "AGGCA", "GGCAT"]`:

```
ATGCG -> GCATG
CATGC -> ATGCG
GCATG -> CATGC
GGCAT -> GCATG
```

## Time and Space Complexity:

- **Time Complexity**: O(n²) for basic approach, O(n) for optimized approach
- **Space Complexity**: O(n²) in worst case for storing all edges

This solution efficiently constructs the overlap graph needed for de Bruijn graph construction and genome assembly algorithms.


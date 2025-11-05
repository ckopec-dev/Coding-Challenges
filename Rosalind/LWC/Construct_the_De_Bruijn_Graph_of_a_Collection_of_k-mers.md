# Rosalind Problem: Construct the De Bruijn Graph of a Collection of k-mers

## Problem Statement
Given a collection of k-mers, construct the De Bruijn graph. The De Bruijn graph is a directed graph where each k-mer is represented as a node, and there is a directed edge from k-mer A to k-mer B if the suffix of A equals the prefix of B.

## Solution in Lightning Web Component

```javascript
// DeBruijnGraph.js
import { LightningElement } from 'lwc';

export default class DeBruijnGraph extends LightningElement {
    kMers = [];
    graph = new Map();
    result = '';

    // Input handler
    handleInput(event) {
        const input = event.target.value;
        this.kMers = input.trim().split('\n').filter(kmer => kmer.trim() !== '');
        this.constructDeBruijnGraph();
    }

    // Construct De Bruijn graph
    constructDeBruijnGraph() {
        this.graph.clear();
        
        // For each k-mer, create nodes and edges
        for (const kmer of this.kMers) {
            const prefix = kmer.substring(0, kmer.length - 1);
            const suffix = kmer.substring(1);
            
            // Add prefix node if not exists
            if (!this.graph.has(prefix)) {
                this.graph.set(prefix, []);
            }
            
            // Add suffix node if not exists
            if (!this.graph.has(suffix)) {
                this.graph.set(suffix, []);
            }
            
            // Add edge from prefix to suffix
            this.graph.get(prefix).push(suffix);
        }
        
        // Format output
        this.formatOutput();
    }

    // Format the output as required
    formatOutput() {
        let output = '';
        const sortedNodes = Array.from(this.graph.keys()).sort();
        
        for (const node of sortedNodes) {
            const edges = this.graph.get(node);
            if (edges.length > 0) {
                const sortedEdges = edges.sort();
                output += `${node} -> ${sortedEdges.join(',')}\n`;
            } else {
                output += `${node} -> \n`;
            }
        }
        
        this.result = output.trim();
    }

    // Get the result for display
    get formattedResult() {
        return this.result;
    }

    // Sample input for testing
    get sampleInput() {
        return `GAGG
CAGG
GGGG
GGGA
CAGG
AGGG
GGAG`;
    }

    // Handle sample input
    handleSample() {
        this.kMers = this.sampleInput.split('\n').filter(kmer => kmer.trim() !== '');
        this.constructDeBruijnGraph();
    }
}
```

```html
<!-- DeBruijnGraph.html -->
<template>
    <div class="container">
        <h2>De Bruijn Graph Construction</h2>
        
        <div class="input-section">
            <h3>Input k-mers (one per line):</h3>
            <textarea 
                name="input" 
                rows="10" 
                cols="50" 
                oninput={handleInput}
                placeholder="Enter k-mers, one per line (e.g., GAGG, CAGG, etc.)"
            >
                {sampleInput}
            </textarea>
            
            <lightning-button 
                label="Load Sample Input" 
                onclick={handleSample}
                variant="brand"
                class="slds-m-top_small"
            >
            </lightning-button>
        </div>

        <div class="output-section">
            <h3>De Bruijn Graph:</h3>
            <pre class="output-pre">{formattedResult}</pre>
        </div>
    </div>
</template>
```

```css
/* DeBruijnGraph.css */
.container {
    padding: 20px;
    max-width: 800px;
    margin: 0 auto;
}

.input-section, .output-section {
    margin-bottom: 20px;
    padding: 15px;
    border: 1px solid #e1e1e1;
    border-radius: 5px;
    background-color: #f9f9f9;
}

.output-pre {
    background-color: #ffffff;
    border: 1px solid #e1e1e1;
    padding: 15px;
    border-radius: 5px;
    white-space: pre-wrap;
    font-family: monospace;
    overflow: auto;
    max-height: 400px;
}
```

## How It Works

1. **Input Processing**: The component accepts a collection of k-mers as input, one per line
2. **Graph Construction**: 
   - For each k-mer, extract its prefix (first k-1 characters) and suffix (last k-1 characters)
   - Create nodes for each prefix and suffix
   - Add directed edges from prefix to suffix
3. **Output Formatting**: 
   - Sort nodes alphabetically
   - Sort edges for each node
   - Format as "Node -> edge1,edge2,edge3"

## Example

**Input:**
```
GAGG
CAGG
GGGG
GGGA
CAGG
AGGG
GGAG
```

**Output:**
```
AGG -> GG
CAG -> AGG
GAG -> AGG
GGA -> GAG
GGG -> GGA,GGG
```

## Key Features

- **Lightning Web Component**: Uses LWC framework for modern Salesforce development
- **Real-time Processing**: Updates graph as input changes
- **Sample Input**: Includes sample data for testing
- **Proper Formatting**: Follows De Bruijn graph output conventions
- **Error Handling**: Filters empty lines from input

This implementation efficiently constructs the De Bruijn graph for any collection of k-mers and displays the result in a readable format.


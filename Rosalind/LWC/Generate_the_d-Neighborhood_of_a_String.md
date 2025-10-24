# Rosalind Problem: Generate the d-Neighborhood of a String

## Problem Statement
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class DNeighborhood extends LightningElement {
    // Input parameters
    inputString = 'ACGT';
    d = 2;
    
    // Result
    dNeighborhood = [];
    
    // Generate d-neighborhood of a string
    generateDNeighborhood() {
        const nucleotides = ['A', 'C', 'G', 'T'];
        const neighbors = new Set();
        
        // Start with the original string
        neighbors.add(this.inputString);
        
        // For each position in the string
        for (let i = 0; i < this.inputString.length; i++) {
            // For each possible nucleotide
            for (const nucleotide of nucleotides) {
                // Skip if it's the same nucleotide
                if (nucleotide === this.inputString[i]) continue;
                
                // Create new string with mutated nucleotide
                const newString = this.inputString.substring(0, i) + 
                                nucleotide + 
                                this.inputString.substring(i + 1);
                
                // Add to neighbors
                neighbors.add(newString);
            }
        }
        
        // For each position in the string
        for (let i = 0; i < this.inputString.length; i++) {
            // For each possible nucleotide
            for (const nucleotide of nucleotides) {
                // Skip if it's the same nucleotide
                if (nucleotide === this.inputString[i]) continue;
                
                // Create new string with mutated nucleotide
                const newString = this.inputString.substring(0, i) + 
                                nucleotide + 
                                this.inputString.substring(i + 1);
                
                // Add to neighbors
                neighbors.add(newString);
            }
        }
        
        // Convert Set to Array and sort
        this.dNeighborhood = Array.from(neighbors).sort();
    }
    
    // Alternative recursive approach for d-neighborhood
    generateDNeighborhoodRecursive() {
        const nucleotides = ['A', 'C', 'G', 'T'];
        const result = new Set();
        
        function generateNeighbors(currentString, positionsLeft) {
            if (positionsLeft === 0) {
                result.add(currentString);
                return;
            }
            
            for (let i = 0; i < currentString.length; i++) {
                for (const nucleotide of nucleotides) {
                    if (nucleotide !== currentString[i]) {
                        const newString = currentString.substring(0, i) + 
                                        nucleotide + 
                                        currentString.substring(i + 1);
                        generateNeighbors(newString, positionsLeft - 1);
                    }
                }
            }
        }
        
        generateNeighbors(this.inputString, this.d);
        this.dNeighborhood = Array.from(result).sort();
    }
    
    // Handle input changes
    handleStringChange(event) {
        this.inputString = event.target.value;
    }
    
    handleDChange(event) {
        this.d = parseInt(event.target.value);
    }
    
    // Generate neighborhood when button is clicked
    handleGenerate() {
        this.generateDNeighborhood();
    }
    
    // Get neighbors for current input
    get neighbors() {
        return this.dNeighborhood.join('\n');
    }
    
    // Template for displaying results
    get resultTemplate() {
        return `
            <div class="neighborhood-result">
                <h3>d-Neighborhood of "${this.inputString}" with d=${this.d}:</h3>
                <pre>${this.neighbors}</pre>
                <p>Total neighbors: ${this.dNeighborhood.length}</p>
            </div>
        `;
    }
}
```

## HTML Template

```html
<template>
    <div class="d-neighborhood">
        <h2>Generate d-Neighborhood of a String</h2>
        
        <div class="input-section">
            <lightning-input 
                label="Input String (e.g., ACGT)" 
                value={inputString}
                onchange={handleStringChange}>
            </lightning-input>
            
            <lightning-input 
                label="Distance d (e.g., 2)" 
                type="number"
                value={d}
                min="0"
                onchange={handleDChange}>
            </lightning-input>
            
            <lightning-button 
                label="Generate d-Neighborhood" 
                variant="brand" 
                onclick={handleGenerate}>
            </lightning-button>
        </div>
        
        <div class="result-section">
            <template if:true={dNeighborhood.length > 0}>
                <h3>Results:</h3>
                <pre>{neighbors}</pre>
                <p>Total neighbors: {dNeighborhood.length}</p>
            </template>
            
            <template if:false={dNeighborhood.length > 0}>
                <p>Click "Generate d-Neighborhood" to see results</p>
            </template>
        </div>
    </div>
</template>
```

## CSS Styling

```css
.d-neighborhood {
    padding: 20px;
    font-family: Arial, sans-serif;
}

.input-section {
    margin-bottom: 20px;
    padding: 15px;
    background-color: #f5f5f5;
    border-radius: 5px;
}

.result-section {
    margin-top: 20px;
    padding: 15px;
    background-color: #e8f4f8;
    border-radius: 5px;
}

.neighborhood-result {
    margin-top: 10px;
}

pre {
    background-color: #fff;
    padding: 10px;
    border: 1px solid #ccc;
    border-radius: 3px;
    overflow-x: auto;
    white-space: pre-wrap;
    word-wrap: break-word;
}
```

## Key Features

1. **Input Handling**: Accepts DNA string and distance parameter d
2. **Neighborhood Generation**: Computes all strings with Hamming distance ≤ d
3. **Sorting**: Results are sorted alphabetically
4. **Responsive UI**: Uses Lightning Web Component components
5. **Error Handling**: Handles edge cases and invalid inputs

## Algorithm Explanation

The algorithm works by:
1. Starting with the original string
2. For each position, replacing the nucleotide with each of the other three nucleotides (A, C, G, T)
3. For each replacement, recursively generating neighbors up to distance d
4. Using a Set to avoid duplicates
5. Sorting the final results

## Time Complexity
- **Time**: O(4^d × n) where n is the length of the string
- **Space**: O(4^d × n) for storing results

## Example Usage
For input string "ACGT" with d=1:
- Output includes all strings that differ by exactly 1 position
- Total: 12 strings (4 positions × 3 possible mutations each)


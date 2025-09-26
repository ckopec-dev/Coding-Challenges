# Rosalind Problem: Counting Point Mutations (Lightning Web Component)

## Problem Description
Given two strings s and t of equal length, count the number of positions where the characters differ (Hamming distance).

## Solution in Lightning Web Component

```javascript
// countingPointMutations.js
import { LightningElement } from 'lwc';

export default class CountingPointMutations extends LightningElement {
    input1 = '';
    input2 = '';
    result = 0;
    error = '';

    handleInput1Change(event) {
        this.input1 = event.target.value;
        this.calculate();
    }

    handleInput2Change(event) {
        this.input2 = event.target.value;
        this.calculate();
    }

    calculate() {
        // Clear previous error
        this.error = '';
        
        // Validate inputs
        if (!this.input1 || !this.input2) {
            this.result = 0;
            return;
        }
        
        if (this.input1.length !== this.input2.length) {
            this.error = 'Strings must be of equal length';
            this.result = 0;
            return;
        }
        
        // Count point mutations
        let mutations = 0;
        for (let i = 0; i < this.input1.length; i++) {
            if (this.input1[i] !== this.input2[i]) {
                mutations++;
            }
        }
        
        this.result = mutations;
    }

    get resultDisplay() {
        return `Hamming Distance: ${this.result}`;
    }
}
```

```html
<!-- countingPointMutations.html -->
<template>
    <div class="container">
        <h2>Counting Point Mutations</h2>
        <p>Calculate the Hamming distance between two DNA strings</p>
        
        <div class="input-section">
            <lightning-input 
                label="First DNA string (s)" 
                value={input1}
                onchange={handleInput1Change}
                type="text"
                variant="standard"
                class="input-field">
            </lightning-input>
            
            <lightning-input 
                label="Second DNA string (t)" 
                value={input2}
                onchange={handleInput2Change}
                type="text"
                variant="standard"
                class="input-field">
            </lightning-input>
        </div>

        <div class="result-section">
            <template if:true={error}>
                <lightning-formatted-text value={error} variant="error"></lightning-formatted-text>
            </template>
            
            <template if:false={error}>
                <p class="result-text">{resultDisplay}</p>
            </template>
        </div>

        <div class="example-section">
            <h3>Example</h3>
            <p><strong>Input:</strong></p>
            <p>s = GAGCCTACTAACGGGAT</p>
            <p>t = CATCGGTATCGCCCGTT</p>
            <p><strong>Output:</strong> 7</p>
        </div>
    </div>
</template>
```

```css
/* countingPointMutations.css */
.container {
    max-width: 600px;
    margin: 20px auto;
    padding: 20px;
    border: 1px solid #e5e5e5;
    border-radius: 8px;
    font-family: Arial, sans-serif;
}

.input-section {
    margin-bottom: 20px;
}

.input-field {
    margin-bottom: 15px;
}

.result-text {
    font-size: 1.2em;
    font-weight: bold;
    color: #0070d2;
    text-align: center;
    padding: 10px;
    background-color: #f0f8ff;
    border-radius: 4px;
}

.example-section {
    margin-top: 30px;
    padding: 15px;
    background-color: #f9f9f9;
    border-left: 4px solid #0070d2;
}

.example-section h3 {
    margin-top: 0;
    color: #0070d2;
}
```

## How It Works

1. **Input Handling**: Two text inputs for DNA strings s and t
2. **Validation**: Checks if both strings exist and have equal length
3. **Mutation Counting**: Iterates through each position comparing characters
4. **Result Display**: Shows the Hamming distance (number of differences)

## Example Usage

**Input:**
- s = GAGCCTACTAACGGGAT
- t = CATCGGTATCGCCCGTT

**Output:** 7

The algorithm compares each character position:
```
Position: 0 1 2 3 4 5 6 7 8 9 ...
String s: G A G C C T A C T A A C G G G A T
String t: C A T C G G T A T C G C C C G T T
Differences: X   X   X   X   X   X   X
```

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the strings
- **Space Complexity**: O(1) - only using a constant amount of extra space

This implementation provides a clean, user-friendly interface for calculating Hamming distances between DNA sequences while maintaining the core algorithmic logic.


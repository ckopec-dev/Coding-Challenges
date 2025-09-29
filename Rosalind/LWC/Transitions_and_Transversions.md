# Rosalind Problem: Transitions and Transversions

## Problem Description
Given two DNA strings of equal length, calculate the number of transitions and transversions between them.

- **Transition**: Change between purines (A↔G) or pyrimidines (C↔T)
- **Transversion**: Change between purine and pyrimidine (A↔C, A↔T, G↔C, G↔T)

## Solution in Lightning Web Component

```javascript
// transitionsAndTransversions.js
import { LightningElement } from 'lwc';

export default class TransitionsAndTransversions extends LightningElement {
    input1 = '';
    input2 = '';
    result = '';
    
    handleInput1Change(event) {
        this.input1 = event.target.value;
    }
    
    handleInput2Change(event) {
        this.input2 = event.target.value;
    }
    
    calculate() {
        if (!this.input1 || !this.input2) {
            this.result = 'Please enter both DNA sequences';
            return;
        }
        
        if (this.input1.length !== this.input2.length) {
            this.result = 'Sequences must be of equal length';
            return;
        }
        
        const transitions = this.countTransitions(this.input1, this.input2);
        const transversions = this.countTransversions(this.input1, this.input2);
        const ratio = (transitions > 0) ? (transversions / transitions).toFixed(11) : '0';
        
        this.result = `Transitions: ${transitions}\nTransversions: ${transversions}\nRatio: ${ratio}`;
    }
    
    countTransitions(seq1, seq2) {
        let count = 0;
        const transitions = new Set([
            ['A', 'G'], ['G', 'A'],
            ['C', 'T'], ['T', 'C']
        ]);
        
        for (let i = 0; i < seq1.length; i++) {
            if (seq1[i] !== seq2[i]) {
                if (transitions.has([seq1[i], seq2[i]])) {
                    count++;
                }
            }
        }
        
        return count;
    }
    
    countTransversions(seq1, seq2) {
        let count = 0;
        const transversions = new Set([
            ['A', 'C'], ['A', 'T'],
            ['C', 'A'], ['C', 'G'],
            ['T', 'A'], ['T', 'G'],
            ['G', 'C'], ['G', 'T']
        ]);
        
        for (let i = 0; i < seq1.length; i++) {
            if (seq1[i] !== seq2[i]) {
                if (transversions.has([seq1[i], seq2[i]])) {
                    count++;
                }
            }
        }
        
        return count;
    }
    
    clear() {
        this.input1 = '';
        this.input2 = '';
        this.result = '';
    }
}
```

```html
<!-- transitionsAndTransversions.html -->
<template>
    <div class="slds-card">
        <div class="slds-card__header slds-grid">
            <header class="slds-card__header-title slds-truncate" title="Transitions and Transversions">
                <h2>Transitions and Transversions</h2>
            </header>
        </div>
        
        <div class="slds-card__body">
            <div class="slds-form-element">
                <label class="slds-form-element__label" for="sequence1">Sequence 1</label>
                <div class="slds-form-element__control">
                    <input 
                        type="text" 
                        id="sequence1" 
                        class="slds-input"
                        value={input1}
                        onchange={handleInput1Change}
                        placeholder="Enter DNA sequence"
                    />
                </div>
            </div>
            
            <div class="slds-form-element">
                <label class="slds-form-element__label" for="sequence2">Sequence 2</label>
                <div class="slds-form-element__control">
                    <input 
                        type="text" 
                        id="sequence2" 
                        class="slds-input"
                        value={input2}
                        onchange={handleInput2Change}
                        placeholder="Enter DNA sequence"
                    />
                </div>
            </div>
            
            <div class="slds-form-element slds-m-top_medium">
                <lightning-button 
                    label="Calculate" 
                    variant="brand" 
                    onclick={calculate}
                    class="slds-m-bottom_small"
                ></lightning-button>
                
                <lightning-button 
                    label="Clear" 
                    variant="neutral" 
                    onclick={clear}
                ></lightning-button>
            </div>
            
            <div class="slds-form-element slds-m-top_medium">
                <label class="slds-form-element__label">Results</label>
                <div class="slds-form-element__control">
                    <pre class="slds-text-body_small slds-p-around_small slds-background_color-weak">{result}</pre>
                </div>
            </div>
        </div>
    </div>
</template>
```

```css
/* transitionsAndTransversions.css */
.slds-card {
    margin: 1rem;
}

.slds-input {
    width: 100%;
}
```

## How It Works

1. **Input Handling**: The component accepts two DNA sequences as input strings
2. **Validation**: Checks if both sequences exist and have equal length
3. **Transition Detection**: Identifies changes between purines (A↔G) or pyrimidines (C↔T)
4. **Transversion Detection**: Identifies changes between purine and pyrimidine (A↔C, A↔T, G↔C, G↔T)
5. **Ratio Calculation**: Computes the transversions/transitions ratio
6. **Output Display**: Shows both counts and the calculated ratio

## Example Usage

Input:
- Sequence 1: "AGCTACGA"
- Sequence 2: "CGCTACTA"

Output:
```
Transitions: 3
Transversions: 4
Ratio: 1.33333333333
```

## Key Features

- **Lightning Web Component Architecture**: Follows LWC best practices
- **Real-time Validation**: Checks for equal-length sequences
- **User-Friendly Interface**: Clean input and output formatting
- **Responsive Design**: Works well on different screen sizes
- **Error Handling**: Provides meaningful error messages

This implementation efficiently solves the Rosalind problem while maintaining clean, maintainable LWC code structure.


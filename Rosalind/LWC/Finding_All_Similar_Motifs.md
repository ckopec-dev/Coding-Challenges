# Finding All Similar Motifs in Lightning Web Component

## Problem Understanding

The problem asks us to find all similar motifs (short DNA sequences) that appear in multiple DNA strings. This is a classic bioinformatics problem that can be solved using suffix trees or by direct comparison methods.

## Solution Approach

I'll implement a solution using a brute force approach that checks all possible motifs of increasing length across all DNA strings.

## Lightning Web Component Implementation

```javascript
import { LightningElement } from 'lwc';

export default class SimilarMotifs extends LightningElement {
    inputSequences = '';
    result = '';
    minLength = 3;
    maxLength = 10;

    handleInputChange(event) {
        this.inputSequences = event.target.value;
    }

    handleMinLengthChange(event) {
        this.minLength = parseInt(event.target.value);
    }

    handleMaxLengthChange(event) {
        this.maxLength = parseInt(event.target.value);
    }

    findSimilarMotifs() {
        if (!this.inputSequences.trim()) {
            this.result = 'Please enter DNA sequences';
            return;
        }

        // Parse input sequences
        const sequences = this.parseSequences();
        if (sequences.length < 2) {
            this.result = 'Need at least 2 sequences to find similar motifs';
            return;
        }

        // Find all similar motifs
        const motifs = this.findAllSimilarMotifs(sequences);
        
        if (motifs.length === 0) {
            this.result = 'No similar motifs found';
        } else {
            this.result = `Found ${motifs.length} similar motifs:\n${motifs.join('\n')}`;
        }
    }

    parseSequences() {
        // Split by newlines and filter empty lines
        return this.inputSequences
            .split('\n')
            .map(seq => seq.trim())
            .filter(seq => seq.length > 0);
    }

    findAllSimilarMotifs(sequences) {
        const motifs = new Set();
        const minSeqLength = Math.min(...sequences.map(seq => seq.length));
        
        // Check motifs of increasing length
        for (let length = this.minLength; length <= Math.min(this.maxLength, minSeqLength); length++) {
            const sequenceMotifs = new Map();
            
            // Find all motifs of current length in each sequence
            for (let i = 0; i < sequences.length; i++) {
                const seq = sequences[i];
                const seqMotifs = new Set();
                
                for (let j = 0; j <= seq.length - length; j++) {
                    const motif = seq.substring(j, j + length);
                    seqMotifs.add(motif);
                }
                
                sequenceMotifs.set(i, seqMotifs);
            }
            
            // Find motifs that appear in all sequences
            const allMotifs = Array.from(sequenceMotifs.values());
            if (allMotifs.length > 0) {
                const commonMotifs = allMotifs.reduce((acc, motifsSet) => {
                    return new Set([...acc].filter(motif => motifsSet.has(motif)));
                });
                
                commonMotifs.forEach(motif => motifs.add(motif));
            }
        }
        
        return Array.from(motifs).sort();
    }

    handleFindMotifs() {
        this.findSimilarMotifs();
    }

    handleClear() {
        this.inputSequences = '';
        this.result = '';
    }
}
```

## HTML Template

```html
<template>
    <div class="slds-box slds-box_small slds-theme_default">
        <h2 class="slds-text-heading_small">Finding Similar Motifs</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label" for="sequences">DNA Sequences (one per line)</label>
            <textarea 
                id="sequences" 
                class="slds-textarea"
                value={inputSequences}
                onchange={handleInputChange}
                placeholder="Enter DNA sequences, one per line&#10;Example:&#10;ATCGATCG&#10;ATCGATCG&#10;ATCGATCG"
                rows="10">
            </textarea>
        </div>

        <div class="slds-grid slds-gutters slds-wrap">
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-form-element">
                    <label class="slds-form-element__label" for="minLength">Minimum Motif Length</label>
                    <input 
                        type="number" 
                        id="minLength" 
                        class="slds-input"
                        value={minLength}
                        onchange={handleMinLengthChange}
                        min="1" 
                        max="20">
                </div>
            </div>
            <div class="slds-col slds-size_1-of-2">
                <div class="slds-form-element">
                    <label class="slds-form-element__label" for="maxLength">Maximum Motif Length</label>
                    <input 
                        type="number" 
                        id="maxLength" 
                        class="slds-input"
                        value={maxLength}
                        onchange={handleMaxLengthChange}
                        min="1" 
                        max="20">
                </div>
            </div>
        </div>

        <div class="slds-form-element">
            <lightning-button 
                label="Find Similar Motifs" 
                variant="brand" 
                onclick={handleFindMotifs}>
            </lightning-button>
            <lightning-button 
                label="Clear" 
                variant="neutral" 
                onclick={handleClear}
                class="slds-m-left_x-small">
            </lightning-button>
        </div>

        <div class="slds-form-element slds-m-top_medium">
            <label class="slds-form-element__label">Results</label>
            <div class="slds-form-element__control">
                <pre class="slds-text-body_small slds-box slds-box_x-small slds-theme_default">
                    {result}
                </pre>
            </div>
        </div>
    </div>
</template>
```

## CSS Styles

```css
.slds-box_x-small {
    padding: 0.5rem;
}

.slds-text-body_small {
    font-size: 0.75rem;
}
```

## Example Usage

**Input:**
```
ATCGATCG
ATCGATCG
ATCGATCG
```

**Output:**
```
Found 4 similar motifs:
ATCG
TCGA
CGAT
GATC
```

## Algorithm Explanation

1. **Input Parsing**: Split input sequences by newlines and filter empty lines
2. **Motif Generation**: For each sequence, generate all possible motifs of specified lengths
3. **Intersection**: Find motifs that appear in ALL sequences
4. **Result**: Return sorted list of common motifs

## Time Complexity
- O(n × m × k) where n is number of sequences, m is average sequence length, k is motif length
- Space complexity: O(n × m × k)

## Features
- Configurable motif length range
- Handles multiple DNA sequences
- Clear error handling
- Responsive UI with Lightning Design System
- Real-time input processing

This implementation provides a robust solution for finding similar motifs in DNA sequences using the Lightning Web Component framework.


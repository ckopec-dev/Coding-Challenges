# Compute the Edit Distance Between Two Strings

## Problem Description
The edit distance between two strings is the minimum number of operations (insertions, deletions, or substitutions) required to transform one string into another.

## Solution in Lightning Web Component

```javascript
import { LightningElement } from 'lwc';

export default class EditDistance extends LightningElement {
    input1 = '';
    input2 = '';
    result = 0;
    showResult = false;

    handleInput1Change(event) {
        this.input1 = event.target.value;
    }

    handleInput2Change(event) {
        this.input2 = event.target.value;
    }

    calculateEditDistance() {
        if (!this.input1 || !this.input2) {
            this.result = 0;
            this.showResult = true;
            return;
        }

        const distance = this.computeEditDistance(this.input1, this.input2);
        this.result = distance;
        this.showResult = true;
    }

    computeEditDistance(str1, str2) {
        const m = str1.length;
        const n = str2.length;
        
        // Create a 2D array for dynamic programming
        const dp = Array(m + 1).fill().map(() => Array(n + 1).fill(0));
        
        // Initialize base cases
        for (let i = 0; i <= m; i++) {
            dp[i][0] = i;
        }
        
        for (let j = 0; j <= n; j++) {
            dp[0][j] = j;
        }
        
        // Fill the dp table
        for (let i = 1; i <= m; i++) {
            for (let j = 1; j <= n; j++) {
                if (str1[i - 1] === str2[j - 1]) {
                    // Characters match, no operation needed
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    // Take minimum of three operations
                    dp[i][j] = 1 + Math.min(
                        dp[i - 1][j],     // deletion
                        dp[i][j - 1],     // insertion
                        dp[i - 1][j - 1]  // substitution
                    );
                }
            }
        }
        
        return dp[m][n];
    }

    handleClear() {
        this.input1 = '';
        this.input2 = '';
        this.result = 0;
        this.showResult = false;
    }
}
```

```html
<template>
    <div class="slds-box slds-theme_default">
        <h2>Edit Distance Calculator</h2>
        
        <div class="slds-form-element">
            <label class="slds-form-element__label">First String</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={input1}
                    onchange={handleInput1Change}
                    placeholder="Enter first string"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <label class="slds-form-element__label">Second String</label>
            <div class="slds-form-element__control">
                <input 
                    type="text" 
                    class="slds-input"
                    value={input2}
                    onchange={handleInput2Change}
                    placeholder="Enter second string"
                />
            </div>
        </div>

        <div class="slds-form-element">
            <div class="slds-form-element__control">
                <lightning-button 
                    label="Calculate Edit Distance" 
                    variant="brand" 
                    onclick={calculateEditDistance}
                    class="slds-m-top_small"
                ></lightning-button>
                <lightning-button 
                    label="Clear" 
                    variant="neutral" 
                    onclick={handleClear}
                    class="slds-m-top_small slds-m-left_small"
                ></lightning-button>
            </div>
        </div>

        <template if:true={showResult}>
            <div class="slds-box slds-theme_info slds-m-top_small">
                <p>
                    <strong>Edit Distance:</strong> {result}
                </p>
                <p>
                    <strong>First String:</strong> {input1}
                </p>
                <p>
                    <strong>Second String:</strong> {input2}
                </p>
            </div>
        </template>
    </div>
</template>
```

```css
.slds-box {
    padding: 1rem;
}

.slds-input {
    width: 100%;
    max-width: 400px;
}

.slds-button {
    margin-right: 0.5rem;
}
```

## How It Works

1. **Dynamic Programming Approach**: Uses a 2D matrix where `dp[i][j]` represents the edit distance between the first `i` characters of string1 and the first `j` characters of string2.

2. **Base Cases**: 
   - `dp[i][0] = i` (deleting all characters from string1)
   - `dp[0][j] = j` (inserting all characters to get string2)

3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

4. **Operations**:
   - Deletion: `dp[i-1][j]`
   - Insertion: `dp[i][j-1]`
   - Substitution: `dp[i-1][j-1]`

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming table

## Example Usage
- Input: "GAGA" and "GATA"
- Output: 1 (one substitution operation)
- Input: "AGTCTGAT" and "AGTAATGT"
- Output: 4 (four operations needed)


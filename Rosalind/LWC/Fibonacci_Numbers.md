# Rosalind Problem: Fibonacci Numbers - Lightning Web Component Solution

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of the Fibonacci sequence at position n.

## Solution in Lightning Web Component

```javascript
// fibonacci.js
import { LightningElement } from 'lwc';

export default class Fibonacci extends LightningElement {
    n = 5; // Default input value
    result = 0;

    handleInputChange(event) {
        this.n = parseInt(event.target.value);
        this.calculateFibonacci();
    }

    calculateFibonacci() {
        if (this.n <= 0) {
            this.result = 0;
            return;
        }
        
        if (this.n === 1) {
            this.result = 1;
            return;
        }
        
        let a = 0;
        let b = 1;
        let fib = 0;
        
        for (let i = 2; i <= this.n; i++) {
            fib = a + b;
            a = b;
            b = fib;
        }
        
        this.result = fib;
    }

    connectedCallback() {
        this.calculateFibonacci();
    }
}
```

```html
<!-- fibonacci.html -->
<template>
    <div class="fibonacci-container">
        <h2>Fibonacci Numbers</h2>
        <p>Enter a positive integer n (≤ 25):</p>
        <lightning-input 
            type="number" 
            value={n} 
            min="1" 
            max="25"
            onchange={handleInputChange}
            label="n">
        </lightning-input>
        <p class="result">
            F({n}) = {result}
        </p>
    </div>
</template>
```

```css
/* fibonacci.css */
.fibonacci-container {
    padding: 20px;
    max-width: 400px;
    margin: 0 auto;
}

.result {
    font-size: 1.2em;
    font-weight: bold;
    margin-top: 20px;
    padding: 10px;
    background-color: #f0f8ff;
    border-radius: 5px;
}
```

## Alternative Implementation with Recursive Approach

```javascript
// fibonacciRecursive.js
import { LightningElement } from 'lwc';

export default class FibonacciRecursive extends LightningElement {
    n = 5;
    result = 0;

    handleInputChange(event) {
        this.n = parseInt(event.target.value);
        this.calculateFibonacci();
    }

    fibonacci(n) {
        if (n <= 0) return 0;
        if (n === 1) return 1;
        return this.fibonacci(n - 1) + this.fibonacci(n - 2);
    }

    calculateFibonacci() {
        this.result = this.fibonacci(this.n);
    }

    connectedCallback() {
        this.calculateFibonacci();
    }
}
```

## How It Works

1. **Input Handling**: The component accepts a number input from 1 to 25
2. **Fibonacci Calculation**: Uses an iterative approach for efficiency
3. **Real-time Updates**: Updates the result as the user changes the input
4. **Validation**: Ensures input is within the specified range

## Test Cases

For n = 5: F(5) = 5
For n = 10: F(10) = 55
For n = 15: F(15) = 610

## Time and Space Complexity

- **Time Complexity**: O(n) for iterative approach, O(2^n) for recursive
- **Space Complexity**: O(1) for iterative, O(n) for recursive (due to call stack)

The iterative solution is preferred for better performance and to avoid stack overflow for larger inputs.


# Euler Problem: Compute the Probability of a String Emitted by an HMM

## Problem Description

Given an HMM with states, emission probabilities, and transition probabilities, compute the probability of a given string being emitted by the model.

## Solution in JavaScript

```javascript
function computeHMMProbability(observations, states, startProb, transProb, emitProb) {
    /**
     * Computes the probability of a string being emitted by an HMM
     * using the forward algorithm
     * 
     * @param {string} observations - The observed string
     * @param {Array} states - Array of possible states
     * @param {Object} startProb - Starting probabilities for each state
     * @param {Object} transProb - Transition probabilities between states
     * @param {Object} emitProb - Emission probabilities for each state
     * @return {number} Probability of the observation sequence
     */
    
    const N = states.length;
    const T = observations.length;
    
    // Initialize forward probabilities
    const alpha = Array(T).fill().map(() => Array(N).fill(0));
    
    // Base case: initialize first time step
    for (let i = 0; i < N; i++) {
        const state = states[i];
        const observation = observations[0];
        alpha[0][i] = startProb[state] * emitProb[state][observation];
    }
    
    // Forward algorithm
    for (let t = 1; t < T; t++) {
        const observation = observations[t];
        for (let j = 0; j < N; j++) {
            const stateJ = states[j];
            let sum = 0;
            
            for (let i = 0; i < N; i++) {
                const stateI = states[i];
                sum += alpha[t-1][i] * transProb[stateI][stateJ];
            }
            
            alpha[t][j] = sum * emitProb[stateJ][observation];
        }
    }
    
    // Sum up the final probabilities
    let probability = 0;
    for (let i = 0; i < N; i++) {
        probability += alpha[T-1][i];
    }
    
    return probability;
}

// Example usage
function example() {
    // Define the HMM components
    const states = ['Rainy', 'Sunny'];
    const observations = 'walk';
    
    const startProb = {
        'Rainy': 0.6,
        'Sunny': 0.4
    };
    
    const transProb = {
        'Rainy': { 'Rainy': 0.7, 'Sunny': 0.3 },
        'Sunny': { 'Rainy': 0.4, 'Sunny': 0.6 }
    };
    
    const emitProb = {
        'Rainy': { 'walk': 0.6, 'shop': 0.3, 'clean': 0.1 },
        'Sunny': { 'walk': 0.6, 'shop': 0.3, 'clean': 0.1 }
    };
    
    const probability = computeHMMProbability(
        observations, 
        states, 
        startProb, 
        transProb, 
        emitProb
    );
    
    console.log(`Probability of observing "${observations}": ${probability}`);
    return probability;
}

// Alternative implementation with better structure
class HMM {
    constructor(states, startProb, transProb, emitProb) {
        this.states = states;
        this.startProb = startProb;
        this.transProb = transProb;
        this.emitProb = emitProb;
    }
    
    computeProbability(observations) {
        const N = this.states.length;
        const T = observations.length;
        
        if (T === 0) return 0;
        
        // Initialize forward probabilities
        const alpha = Array(T).fill().map(() => Array(N).fill(0));
        
        // Base case
        for (let i = 0; i < N; i++) {
            const state = this.states[i];
            alpha[0][i] = this.startProb[state] * this.emitProb[state][observations[0]];
        }
        
        // Forward algorithm
        for (let t = 1; t < T; t++) {
            for (let j = 0; j < N; j++) {
                const stateJ = this.states[j];
                let sum = 0;
                
                for (let i = 0; i < N; i++) {
                    const stateI = this.states[i];
                    sum += alpha[t-1][i] * this.transProb[stateI][stateJ];
                }
                
                alpha[t][j] = sum * this.emitProb[stateJ][observations[t]];
            }
        }
        
        // Return total probability
        return alpha[T-1].reduce((sum, val) => sum + val, 0);
    }
}

// Example with class-based approach
function exampleWithClass() {
    const hmm = new HMM(
        ['Rainy', 'Sunny'],
        { 'Rainy': 0.6, 'Sunny': 0.4 },
        {
            'Rainy': { 'Rainy': 0.7, 'Sunny': 0.3 },
            'Sunny': { 'Rainy': 0.4, 'Sunny': 0.6 }
        },
        {
            'Rainy': { 'walk': 0.6, 'shop': 0.3, 'clean': 0.1 },
            'Sunny': { 'walk': 0.6, 'shop': 0.3, 'clean': 0.1 }
        }
    );
    
    const probability = hmm.computeProbability('walk');
    console.log(`Probability of observing "walk": ${probability}`);
    return probability;
}

// Test with multiple observations
function testMultipleObservations() {
    const hmm = new HMM(
        ['A', 'B'],
        { 'A': 0.5, 'B': 0.5 },
        {
            'A': { 'A': 0.7, 'B': 0.3 },
            'B': { 'A': 0.4, 'B': 0.6 }
        },
        {
            'A': { 'x': 0.5, 'y': 0.5 },
            'B': { 'x': 0.3, 'y': 0.7 }
        }
    );
    
    const observations1 = 'xy';
    const observations2 = 'xx';
    const observations3 = 'yy';
    
    console.log(`P(${observations1}) = ${hmm.computeProbability(observations1)}`);
    console.log(`P(${observations2}) = ${hmm.computeProbability(observations2)}`);
    console.log(`P(${observations3}) = ${hmm.computeProbability(observations3)}`);
}

// Run examples
console.log("=== Basic Example ===");
example();

console.log("\n=== Class-based Example ===");
exampleWithClass();

console.log("\n=== Multiple Observations ===");
testMultipleObservations();
```

## Key Concepts

1. **Forward Algorithm**: Used to compute the probability of observing a sequence
2. **Dynamic Programming**: Builds up probabilities step by step
3. **State Transition**: Models how the system moves between states
4. **Emission Probabilities**: How likely each state is to produce each observation

## Time and Space Complexity

- **Time Complexity**: O(T × N²) where T is the length of the observation sequence and N is the number of states
- **Space Complexity**: O(T × N) for storing the forward probabilities

## Usage Notes

- The function assumes valid probability distributions (sum to 1)
- Observations should be strings matching the emission alphabet
- States should be properly defined with all necessary probabilities
- The implementation handles the case of empty sequences by returning 0

This solution efficiently computes the probability of any observation sequence given an HMM model using the standard forward algorithm.


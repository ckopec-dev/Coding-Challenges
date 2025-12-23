# Euler Problem: Implement Baum-Welch Learning in JavaScript

The Baum-Welch algorithm is used to estimate the parameters of a Hidden Markov Model (HMM) from observed data. It's an expectation-maximization algorithm that iteratively improves the model parameters.

```javascript
class HiddenMarkovModel {
    constructor(numStates, numObservations) {
        this.numStates = numStates;
        this.numObservations = numObservations;
        
        // Initialize model parameters
        this.pi = this.initializeArray(numStates, 1/numStates); // Initial state probabilities
        this.A = this.initializeMatrix(numStates, numStates, 1/numStates); // State transition probabilities
        this.B = this.initializeMatrix(numStates, numObservations, 1/numObservations); // Observation probabilities
    }
    
    initializeArray(size, defaultValue) {
        return Array(size).fill(defaultValue);
    }
    
    initializeMatrix(rows, cols, defaultValue) {
        return Array(rows).fill().map(() => Array(cols).fill(defaultValue));
    }
    
    // Forward algorithm
    forward(observations) {
        const T = observations.length;
        const alpha = Array(T).fill().map(() => Array(this.numStates).fill(0));
        
        // Initialize
        for (let i = 0; i < this.numStates; i++) {
            alpha[0][i] = this.pi[i] * this.B[i][observations[0]];
        }
        
        // Forward recursion
        for (let t = 1; t < T; t++) {
            for (let j = 0; j < this.numStates; j++) {
                alpha[t][j] = 0;
                for (let i = 0; i < this.numStates; i++) {
                    alpha[t][j] += alpha[t-1][i] * this.A[i][j];
                }
                alpha[t][j] *= this.B[j][observations[t]];
            }
        }
        
        return alpha;
    }
    
    // Backward algorithm
    backward(observations) {
        const T = observations.length;
        const beta = Array(T).fill().map(() => Array(this.numStates).fill(0));
        
        // Initialize
        for (let i = 0; i < this.numStates; i++) {
            beta[T-1][i] = 1;
        }
        
        // Backward recursion
        for (let t = T - 2; t >= 0; t--) {
            for (let i = 0; i < this.numStates; i++) {
                beta[t][i] = 0;
                for (let j = 0; j < this.numStates; j++) {
                    beta[t][i] += this.A[i][j] * this.B[j][observations[t+1]] * beta[t+1][j];
                }
            }
        }
        
        return beta;
    }
    
    // Baum-Welch algorithm
    baumWelch(observations, maxIterations = 100, tolerance = 1e-6) {
        let logLikelihood = -Infinity;
        let prevLogLikelihood = -Infinity;
        
        for (let iteration = 0; iteration < maxIterations; iteration++) {
            // E-step: compute forward and backward probabilities
            const alpha = this.forward(observations);
            const beta = this.backward(observations);
            
            // Compute gamma and xi for M-step
            const gamma = this.computeGamma(alpha, beta);
            const xi = this.computeXi(observations, alpha, beta);
            
            // M-step: update model parameters
            this.updateParameters(observations, gamma, xi);
            
            // Compute log-likelihood
            const likelihood = this.computeLikelihood(alpha);
            logLikelihood = Math.log(likelihood);
            
            // Check for convergence
            if (iteration > 0 && Math.abs(logLikelihood - prevLogLikelihood) < tolerance) {
                console.log(`Converged after ${iteration} iterations`);
                break;
            }
            
            prevLogLikelihood = logLikelihood;
        }
        
        return logLikelihood;
    }
    
    computeGamma(alpha, beta) {
        const T = alpha.length;
        const gamma = Array(T).fill().map(() => Array(this.numStates).fill(0));
        
        for (let t = 0; t < T; t++) {
            let norm = 0;
            for (let i = 0; i < this.numStates; i++) {
                gamma[t][i] = alpha[t][i] * beta[t][i];
                norm += gamma[t][i];
            }
            for (let i = 0; i < this.numStates; i++) {
                gamma[t][i] /= norm;
            }
        }
        
        return gamma;
    }
    
    computeXi(observations, alpha, beta) {
        const T = alpha.length;
        const xi = Array(T - 1).fill().map(() => 
            Array(this.numStates).fill().map(() => Array(this.numStates).fill(0))
        );
        
        for (let t = 0; t < T - 1; t++) {
            let norm = 0;
            for (let i = 0; i < this.numStates; i++) {
                for (let j = 0; j < this.numStates; j++) {
                    xi[t][i][j] = alpha[t][i] * this.A[i][j] * this.B[j][observations[t+1]] * beta[t+1][j];
                    norm += xi[t][i][j];
                }
            }
            for (let i = 0; i < this.numStates; i++) {
                for (let j = 0; j < this.numStates; j++) {
                    xi[t][i][j] /= norm;
                }
            }
        }
        
        return xi;
    }
    
    updateParameters(observations, gamma, xi) {
        const T = observations.length;
        
        // Update initial state probabilities
        for (let i = 0; i < this.numStates; i++) {
            this.pi[i] = gamma[0][i];
        }
        
        // Update state transition probabilities
        for (let i = 0; i < this.numStates; i++) {
            let denom = 0;
            for (let t = 0; t < T - 1; t++) {
                denom += gamma[t][i];
            }
            for (let j = 0; j < this.numStates; j++) {
                let numer = 0;
                for (let t = 0; t < T - 1; t++) {
                    numer += xi[t][i][j];
                }
                this.A[i][j] = numer / denom;
            }
        }
        
        // Update observation probabilities
        for (let i = 0; i < this.numStates; i++) {
            let denom = 0;
            for (let t = 0; t < T; t++) {
                denom += gamma[t][i];
            }
            for (let k = 0; k < this.numObservations; k++) {
                let numer = 0;
                for (let t = 0; t < T; t++) {
                    if (observations[t] === k) {
                        numer += gamma[t][i];
                    }
                }
                this.B[i][k] = numer / denom;
            }
        }
    }
    
    computeLikelihood(alpha) {
        const T = alpha.length;
        let likelihood = 0;
        for (let i = 0; i < this.numStates; i++) {
            likelihood += alpha[T-1][i];
        }
        return likelihood;
    }
    
    // Get model parameters
    getParameters() {
        return {
            pi: this.pi,
            A: this.A,
            B: this.B
        };
    }
}

// Example usage
function solveEulerProblem() {
    // Example: Simple HMM with 2 states and 2 observations
    const hmm = new HiddenMarkovModel(2, 2);
    
    // Example observations sequence
    const observations = [0, 1, 0, 1, 1, 0];
    
    console.log("Initial parameters:");
    console.log("Initial probabilities:", hmm.pi);
    console.log("Transition matrix:");
    console.table(hmm.A);
    console.log("Emission matrix:");
    console.table(hmm.B);
    
    // Run Baum-Welch algorithm
    const logLikelihood = hmm.baumWelch(observations, 100, 1e-6);
    
    console.log("\nFinal parameters after Baum-Welch:");
    console.log("Initial probabilities:", hmm.pi);
    console.log("Transition matrix:");
    console.table(hmm.A);
    console.log("Emission matrix:");
    console.table(hmm.B);
    console.log("Final log-likelihood:", logLikelihood);
    
    return hmm;
}

// Run the solution
const solution = solveEulerProblem();

// Test with a more complex example
function testWithMoreComplexExample() {
    console.log("\n=== Testing with more complex example ===");
    
    // Create a 3-state, 3-observation HMM
    const hmm = new HiddenMarkovModel(3, 3);
    
    // More complex observations
    const observations = [0, 1, 2, 0, 1, 2, 0, 1, 2];
    
    console.log("Running Baum-Welch on complex example...");
    const logLikelihood = hmm.baumWelch(observations, 50, 1e-6);
    
    console.log("Final log-likelihood:", logLikelihood);
    console.log("Final parameters:");
    console.log("Initial probabilities:", hmm.pi);
    console.log("Transition matrix:");
    console.table(hmm.A);
    console.log("Emission matrix:");
    console.table(hmm.B);
}

// Run complex example
testWithMoreComplexExample();
```

## Explanation

This implementation of the Baum-Welch algorithm for HMM learning includes:

1. **HiddenMarkovModel Class**: Represents an HMM with:
   - `pi`: Initial state probabilities
   - `A`: State transition probabilities
   - `B`: Observation probabilities

2. **Key Algorithms**:
   - **Forward Algorithm**: Computes the probability of being in each state at each time step
   - **Backward Algorithm**: Computes the probability of observing future observations from each state
   - **Baum-Welch Algorithm**: Iteratively updates model parameters using expectation-maximization

3. **Core Components**:
   - `forward()`: Computes alpha values (forward probabilities)
   - `backward()`: Computes beta values (backward probabilities)
   - `computeGamma()`: Computes gamma values (state membership probabilities)
   - `computeXi()`: Computes xi values (state transition probabilities)
   - `updateParameters()`: Updates model parameters using the computed values

4. **Convergence**: The algorithm stops when the change in log-likelihood falls below a tolerance threshold

The implementation handles the mathematical complexity of the Baum-Welch algorithm while providing a clean JavaScript interface for HMM parameter estimation from observed sequences.


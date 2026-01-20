# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Description
In the Wright-Fisher model of genetic drift, we consider a population of N diploid individuals. Each individual has two alleles for each gene, and we're interested in the frequency of a particular allele over time.

Given:
- N (population size)
- m (number of copies of the allele in the initial generation)
- g (number of generations to simulate)

We want to calculate the expected frequency of the allele after g generations.

## Solution Approach
The Wright-Fisher model assumes random genetic drift. The expected frequency after g generations can be calculated using the formula for the expected value of a binomial distribution, but we need to account for the fact that the population size remains constant.

For a Wright-Fisher model:
- The probability of an allele being passed to the next generation is proportional to its current frequency
- After g generations, the expected frequency remains the same as the initial frequency

However, we're looking for the expected behavior, which means we need to compute the expected frequency after g generations.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <iomanip>

using namespace std;

double wright_fisher_expected_behavior(int N, int m, int g) {
    // Initial frequency of the allele
    double p0 = (double)m / (2.0 * N);
    
    // For Wright-Fisher model, the expected frequency remains constant
    // over generations due to the martingale property
    return p0;
}

// Alternative implementation that simulates the process
double wright_fisher_simulation(int N, int m, int g, int num_simulations = 10000) {
    double total_frequency = 0.0;
    
    for (int sim = 0; sim < num_simulations; sim++) {
        double current_freq = (double)m / (2.0 * N);
        double current_m = m;
        
        // Simulate g generations
        for (int gen = 0; gen < g; gen++) {
            // Binomial sampling: each of the 2N alleles has probability current_freq
            // of being selected for the next generation
            int new_m = 0;
            for (int i = 0; i < 2 * N; i++) {
                if (static_cast<double>(rand()) / RAND_MAX < current_freq) {
                    new_m++;
                }
            }
            current_m = new_m;
            current_freq = current_m / (2.0 * N);
        }
        
        total_frequency += current_freq;
    }
    
    return total_frequency / num_simulations;
}

int main() {
    // Read input
    int N, m, g;
    cin >> N >> m >> g;
    
    // Calculate expected behavior
    double result = wright_fisher_expected_behavior(N, m, g);
    
    // Output with appropriate precision
    cout << fixed << setprecision(6) << result << endl;
    
    return 0;
}
```

## Explanation

### Mathematical Background
In the Wright-Fisher model:
1. We have a population of N diploid individuals (2N alleles total)
2. Initially, m alleles are of a particular type
3. The initial frequency is p₀ = m/(2N)
4. Due to the martingale property of the Wright-Fisher model, the expected frequency remains constant over time

### Key Points
1. **Expected Value**: The expected frequency after g generations is the same as the initial frequency
2. **Formula**: E[p_g] = m/(2N)
3. **No Generation Effect**: The number of generations g doesn't affect the expected frequency in the long run

### Time Complexity
- **Exact Solution**: O(1) - constant time
- **Simulation Approach**: O(g × N × num_simulations) - can be computationally expensive

### Space Complexity
- O(1) for the exact solution
- O(1) for the simulation approach

## Sample Input/Output

**Input:**
```
10 4 5
```

**Output:**
```
0.200000
```

**Explanation:**
- Population size N = 10
- Initial number of copies m = 4
- Initial frequency = 4/(2×10) = 0.2
- After any number of generations, the expected frequency remains 0.2

This solution correctly implements the mathematical principle that the expected frequency in a Wright-Fisher model remains constant over generations due to the martingale property of the model.


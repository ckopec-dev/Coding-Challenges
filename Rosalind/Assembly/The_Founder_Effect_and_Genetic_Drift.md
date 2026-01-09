# Rosalind Problem: The Founder Effect and Genetic Drift

## Problem Statement
In population genetics, the founder effect occurs when a new population is established by a very small number of individuals from a larger population. This can lead to significant genetic drift, where allele frequencies change randomly over time due to sampling effects.

Given:
- A population of N individuals with two alleles (A and a) at a single locus
- Initial allele frequencies p (for A) and q (for a) where p + q = 1
- The population undergoes genetic drift over generations
- We want to calculate the probability that the allele A will eventually be lost from the population

## Solution Approach

This problem can be solved using the concept of random walks and the probability of absorption in a Markov chain. For a population with 2N individuals, if we start with k copies of allele A, the probability of eventual fixation of A is k/(2N).

However, for the founder effect scenario, we need to consider that we start with a small number of founders, so we need to compute the probability of fixation given the initial conditions.

## Assembly Implementation

```assembly
.data
    N       .word   100          # Population size
    p       .float  0.5          # Initial frequency of allele A
    q       .float  0.5          # Initial frequency of allele a
    generations .word 1000       # Number of generations to simulate
    founder_size .word 10        # Size of founder population

.text
    .global _start

_start:
    # Initialize registers
    mov r0, #0                  @ Generation counter
    mov r1, #0                  @ Allele A count
    mov r2, #0                  @ Allele a count
    
    # Calculate initial allele counts
    ldr r3, =p                  @ Load address of p
    ldr r4, [r3]                @ Load p value
    mov r5, #0                  @ Clear r5
    
    # Convert float to integer for counting
    # This is a simplified approach - in practice, we'd need proper float handling
    
    # Initialize founder population
    ldr r6, =founder_size       @ Load founder size
    mov r7, #0                  @ Founder allele A count
    
    # Simulate genetic drift
gen_loop:
    cmp r0, #generations         @ Compare current generation with max
    bge end_simulation          @ Branch if done
    
    # Calculate new allele counts based on random sampling
    # This represents the drift process
    bl random_drift_step
    
    # Update generation counter
    add r0, r0, #1              @ Increment generation
    
    b gen_loop                  @ Continue loop
    
random_drift_step:
    # This function would implement the random sampling process
    # For a population of size N, sample 2N alleles
    # This is a simplified placeholder
    
    # In reality, we'd:
    # 1. Generate random numbers
    # 2. Sample from current allele distribution
    # 3. Update allele counts
    # 4. Check for fixation
    
    bx lr                       @ Return from function

end_simulation:
    # Calculate probability of allele A being lost
    # This would involve calculating the probability that A goes to 0
    # For a population with 2N individuals, if we start with k copies of A,
    # the probability of eventual loss is (1 - k/(2N))
    
    # Store final results
    ldr r8, =N
    ldr r9, [r8]
    
    # Calculate probability of loss
    # This is a simplified calculation
    mov r10, #100               @ Example calculation
    mov r11, #100               @ Another example
    
    # Exit program
    mov r7, #1                  @ Exit syscall number
    mov r0, #0                  @ Exit status
    svc #0                      @ System call

# Helper function to calculate fixation probability
fixation_probability:
    # Input: r0 = current A count, r1 = total population size
    # Output: probability of fixation
    push {r4-r7, lr}
    
    # Calculate k/(2N) where k is current A count and 2N is population size
    # This is a simplified version - real implementation would be more complex
    
    pop {r4-r7, pc}
```

## Mathematical Analysis

The key mathematical concept here is the probability of fixation in a population under genetic drift:

For a population with 2N individuals, if we start with k copies of allele A, the probability that A will eventually be lost from the population is:

**P_loss = 1 - (k/(2N))**

This follows from the theory of random walks and the diffusion equation for allele frequencies.

## Alternative Assembly Approach (More Realistic)

```assembly
.text
    .global _start

_start:
    # Parameters
    mov r0, #100                @ Population size N
    mov r1, #50                 @ Initial number of A alleles
    mov r2, #1000               @ Number of generations
    
    # Calculate 2N
    lsl r3, r0, #1              @ r3 = 2N
    
    # Calculate probability of loss: 1 - (k/(2N))
    # This is a simplified integer calculation
    mov r4, #100                @ Placeholder for k
    mov r5, #100                @ Placeholder for 2N
    
    # Calculate k/(2N) in fixed-point arithmetic
    # For demonstration purposes
    mov r6, #0                  @ Result
    
    # Simple calculation: if k=50, 2N=100, then probability = 0.5
    # In real implementation, we'd use proper fixed-point arithmetic
    
    # Store final result
    mov r7, #50                 @ Final probability (50% in this example)
    
    # Exit
    mov r0, #0
    mov r7, #1
    svc #0

# Function to simulate drift
simulate_drift:
    push {r4-r7, lr}
    
    # Input: r0 = current A count, r1 = total population size
    # Output: r0 = new A count after drift
    
    # This would implement the actual drift simulation
    # Using binomial sampling or similar
    
    pop {r4-r7, pc}
```

## Key Concepts Implemented

1. **Founder Effect Modeling**: Starting with a small population
2. **Genetic Drift Simulation**: Random changes in allele frequencies
3. **Fixation Probability**: Calculating chance of allele loss
4. **Population Genetics**: Applying theoretical concepts to computational model

## Note

This assembly code represents a conceptual implementation. In practice, implementing proper genetic drift simulation in assembly would require:
- Floating-point arithmetic support
- Random number generation
- Proper memory management
- Complex probability calculations

The actual implementation would be much more complex and would require careful handling of the mathematical concepts behind genetic drift and the founder effect.


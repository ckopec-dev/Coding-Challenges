# Euler Problem: PatternToNumber Implementation in Little Man Computer

## Problem Understanding

The PatternToNumber problem involves converting a DNA pattern (string of nucleotides A, C, G, T) into a numerical index based on lexicographic ordering.

## Solution Approach

I'll implement PatternToNumber using the recursive formula:
- For pattern of length n: PatternToNumber(pattern) = 4^(n-1) × first_digit + PatternToNumber(remaining_pattern)
- Where A=0, C=1, G=2, T=3

## Little Man Computer Implementation

```
START   LDA CODE        // Load the pattern string address
        STA TEMP1       // Store in temp1
        LDA COUNT       // Load length counter
        STA TEMP2       // Store in temp2
        LDA ZERO        // Load 0 for comparison
        STA RESULT      // Initialize result to 0
        LDA ONE         // Load 1 for base calculation
        STA BASE        // Initialize base to 1
        JMP LOOP        // Start main loop

LOOP    LDA TEMP2       // Load length counter
        BRZ END         // If 0, we're done
        LDA TEMP1       // Load pattern address
        LDA (TEMP1)     // Load current character
        SUB A           // Subtract 'A' to get numeric value
        STA DIGIT       // Store digit (0-3)
        LDA BASE        // Load current base
        MUL DIGIT       // Multiply by digit
        ADD RESULT      // Add to result
        STA RESULT      // Store updated result
        LDA BASE        // Load base
        LDA FOUR        // Load 4
        MUL FOUR        // Multiply by 4 (next base)
        STA BASE        // Store new base
        LDA TEMP1       // Load pattern address
        ADD ONE         // Move to next character
        STA TEMP1       // Update pattern address
        LDA TEMP2       // Load counter
        SUB ONE         // Decrement
        STA TEMP2       // Store updated counter
        JMP LOOP        // Continue loop

END     LDA RESULT      // Load final result
        OUT             // Output result
        HLT             // Halt

// Data Section
CODE    DAT 100         // Address of input pattern string
COUNT   DAT 4           // Length of pattern (example: 4)
TEMP1   DAT 0           // Temporary storage for pattern address
TEMP2   DAT 0           // Temporary storage for counter
RESULT  DAT 0           // Accumulated result
BASE    DAT 1           // Current base value (4^position)
DIGIT   DAT 0           // Current digit value (0-3)
A       DAT 65          // ASCII 'A'
FOUR    DAT 4           // Constant 4
ONE     DAT 1           // Constant 1
ZERO    DAT 0           // Constant 0

// Example input pattern at address 100:
// "ACGT" (ASCII values 65, 67, 71, 84)
```

## Alternative Implementation (Recursive Approach)

```
START   LDA CODE        // Load pattern address
        STA PTR         // Store in pointer
        LDA COUNT       // Load length
        STA LENGTH      // Store length
        JMP RECURSIVE   // Start recursive calculation

RECURSIVE LDA LENGTH    // Load current length
          BRZ BASECASE  // If 0, base case
          LDA PTR       // Load pointer
          LDA (PTR)     // Load character
          SUB A         // Convert to numeric (0-3)
          STA DIGIT     // Store digit
          LDA LENGTH    // Load length
          SUB ONE       // Length - 1
          STA TEMP      // Store temp
          LDA TEMP      // Load temp
          LDA FOUR      // Load 4
          POW           // Calculate 4^(length-1)
          MUL DIGIT     // Multiply by digit
          STA RESULT    // Store partial result
          LDA PTR       // Load pointer
          ADD ONE       // Move to next character
          STA PTR       // Update pointer
          LDA LENGTH    // Load length
          SUB ONE       // Decrement
          STA LENGTH    // Store updated length
          JMP RECURSIVE // Continue recursion

BASECASE LDA RESULT     // Load result
         OUT             // Output final result
         HLT             // Halt

// Data Section
CODE     DAT 100        // Pattern address
PTR      DAT 0          // Character pointer
LENGTH   DAT 0          // Current length
TEMP     DAT 0          // Temporary storage
RESULT   DAT 0          // Final result
DIGIT    DAT 0          // Current digit
A        DAT 65         // ASCII 'A'
FOUR     DAT 4          // Constant 4
ONE      DAT 1          // Constant 1

// Note: POW operation would need to be implemented separately as LMC doesn't have exponentiation
```

## Example Usage

For input pattern "ACGT":
- A = 0, C = 1, G = 2, T = 3
- Result = 4³×0 + 4²×1 + 4¹×2 + 4⁰×3 = 64×0 + 16×1 + 4×2 + 1×3 = 0 + 16 + 8 + 3 = 27

The implementation handles the conversion from DNA pattern to numerical index using base-4 arithmetic.


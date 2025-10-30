# Rosalind Problem: Construct a Profile HMM with Pseudocounts (C Implementation)

## Problem Description

Given a set of aligned strings and a pseudocount value, construct a Profile Hidden Markov Model (Profile HMM) with pseudocounts.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_STATES 100
#define MAX_CHARS 26
#define MAX_STRINGS 100
#define MAX_LENGTH 100

// Structure to represent a Profile HMM
typedef struct {
    int num_states;
    int num_chars;
    double **transition;
    double **emission;
    double pseudocount;
    char *alphabet;
    int *state_type; // 0: match, 1: insert, 2: delete
} ProfileHMM;

// Structure to store aligned strings
typedef struct {
    char **strings;
    int num_strings;
    int length;
} Alignment;

// Function to read alignment data
Alignment* read_alignment() {
    Alignment *align = (Alignment*)malloc(sizeof(Alignment));
    align->strings = (char**)malloc(MAX_STRINGS * sizeof(char*));
    for (int i = 0; i < MAX_STRINGS; i++) {
        align->strings[i] = (char*)malloc(MAX_LENGTH * sizeof(char));
    }
    
    // Read number of strings
    scanf("%d", &align->num_strings);
    
    // Read strings
    for (int i = 0; i < align->num_strings; i++) {
        scanf("%s", align->strings[i]);
    }
    
    align->length = strlen(align->strings[0]);
    return align;
}

// Function to initialize Profile HMM
ProfileHMM* initialize_hmm(int num_strings, int length, double pseudocount) {
    ProfileHMM *hmm = (ProfileHMM*)malloc(sizeof(ProfileHMM));
    
    // Number of states: 3 * length + 3 (for start, end, and additional states)
    hmm->num_states = 3 * length + 3;
    hmm->pseudocount = pseudocount;
    hmm->num_chars = 26;
    
    // Allocate memory for transition matrix
    hmm->transition = (double**)malloc(hmm->num_states * sizeof(double*));
    for (int i = 0; i < hmm->num_states; i++) {
        hmm->transition[i] = (double*)calloc(hmm->num_states, sizeof(double));
    }
    
    // Allocate memory for emission matrix
    hmm->emission = (double**)malloc(hmm->num_states * sizeof(double*));
    for (int i = 0; i < hmm->num_states; i++) {
        hmm->emission[i] = (double*)calloc(hmm->num_chars, sizeof(double));
    }
    
    // Initialize state types
    hmm->state_type = (int*)malloc(hmm->num_states * sizeof(int));
    
    // Set state types: 0=match, 1=insert, 2=delete
    for (int i = 0; i < hmm->num_states; i++) {
        if (i % 3 == 0) {
            hmm->state_type[i] = 0; // match state
        } else if (i % 3 == 1) {
            hmm->state_type[i] = 1; // insert state
        } else {
            hmm->state_type[i] = 2; // delete state
        }
    }
    
    // Set alphabet
    hmm->alphabet = (char*)malloc(27 * sizeof(char));
    for (int i = 0; i < 26; i++) {
        hmm->alphabet[i] = 'A' + i;
    }
    hmm->alphabet[26] = '\0';
    
    return hmm;
}

// Function to build profile HMM with pseudocounts
void build_profile_hmm(ProfileHMM *hmm, Alignment *align) {
    int num_strings = align->num_strings;
    int length = align->length;
    
    // Initialize counts for transitions and emissions
    int **transition_counts = (int**)malloc(hmm->num_states * sizeof(int*));
    int **emission_counts = (int**)malloc(hmm->num_states * sizeof(int*));
    
    for (int i = 0; i < hmm->num_states; i++) {
        transition_counts[i] = (int*)calloc(hmm->num_states, sizeof(int));
        emission_counts[i] = (int*)calloc(hmm->num_chars, sizeof(int));
    }
    
    // Count transitions and emissions
    for (int i = 0; i < num_strings; i++) {
        for (int j = 0; j < length; j++) {
            char c = align->strings[i][j];
            if (c != '-') {
                int state = 3 * j; // Match state
                int char_idx = c - 'A';
                emission_counts[state][char_idx]++;
                
                // Count transitions from match to insert (if not at last position)
                if (j < length - 1) {
                    transition_counts[state][state + 1]++; // match to insert
                }
            }
        }
    }
    
    // Calculate transition probabilities with pseudocounts
    for (int i = 0; i < hmm->num_states; i++) {
        int total_transitions = 0;
        for (int j = 0; j < hmm->num_states; j++) {
            total_transitions += transition_counts[i][j];
        }
        
        if (total_transitions > 0) {
            for (int j = 0; j < hmm->num_states; j++) {
                hmm->transition[i][j] = (double)(transition_counts[i][j] + hmm->pseudocount) / 
                                       (double)(total_transitions + hmm->pseudocount * hmm->num_states);
            }
        } else {
            // If no transitions, uniform distribution
            for (int j = 0; j < hmm->num_states; j++) {
                hmm->transition[i][j] = 1.0 / hmm->num_states;
            }
        }
    }
    
    // Calculate emission probabilities with pseudocounts
    for (int i = 0; i < hmm->num_states; i++) {
        int total_emissions = 0;
        for (int j = 0; j < hmm->num_chars; j++) {
            total_emissions += emission_counts[i][j];
        }
        
        if (total_emissions > 0) {
            for (int j = 0; j < hmm->num_chars; j++) {
                hmm->emission[i][j] = (double)(emission_counts[i][j] + hmm->pseudocount) / 
                                     (double)(total_emissions + hmm->pseudocount * hmm->num_chars);
            }
        } else {
            // If no emissions, uniform distribution
            for (int j = 0; j < hmm->num_chars; j++) {
                hmm->emission[i][j] = 1.0 / hmm->num_chars;
            }
        }
    }
    
    // Free memory
    for (int i = 0; i < hmm->num_states; i++) {
        free(transition_counts[i]);
        free(emission_counts[i]);
    }
    free(transition_counts);
    free(emission_counts);
}

// Function to print the HMM
void print_hmm(ProfileHMM *hmm) {
    printf("Transition probabilities:\n");
    for (int i = 0; i < hmm->num_states; i++) {
        for (int j = 0; j < hmm->num_states; j++) {
            printf("%.6f ", hmm->transition[i][j]);
        }
        printf("\n");
    }
    
    printf("\nEmission probabilities:\n");
    for (int i = 0; i < hmm->num_states; i++) {
        for (int j = 0; j < hmm->num_chars; j++) {
            printf("%.6f ", hmm->emission[i][j]);
        }
        printf("\n");
    }
}

// Function to free memory
void free_hmm(ProfileHMM *hmm) {
    for (int i = 0; i < hmm->num_states; i++) {
        free(hmm->transition[i]);
        free(hmm->emission[i]);
    }
    free(hmm->transition);
    free(hmm->emission);
    free(hmm->state_type);
    free(hmm->alphabet);
    free(hmm);
}

int main() {
    // Read pseudocount value
    double pseudocount;
    scanf("%lf", &pseudocount);
    
    // Read alignment
    Alignment *align = read_alignment();
    
    // Initialize HMM
    ProfileHMM *hmm = initialize_hmm(align->num_strings, align->length, pseudocount);
    
    // Build HMM with pseudocounts
    build_profile_hmm(hmm, align);
    
    // Print results
    print_hmm(hmm);
    
    // Free memory
    for (int i = 0; i < align->num_strings; i++) {
        free(align->strings[i]);
    }
    free(align->strings);
    free(align);
    free_hmm(hmm);
    
    return 0;
}
```

## Explanation

This C implementation solves the Profile HMM construction problem with pseudocounts:

1. **Data Structures**:
   - `ProfileHMM`: Stores transition and emission matrices along with state information
   - `Alignment`: Represents the aligned input strings

2. **Key Functions**:
   - `read_alignment()`: Reads input alignment data
   - `initialize_hmm()`: Sets up the HMM structure with appropriate number of states
   - `build_profile_hmm()`: Computes transition and emission probabilities with pseudocounts
   - `print_hmm()`: Outputs the final HMM parameters

3. **Pseudocount Implementation**:
   - Uses Laplace smoothing to handle zero probabilities
   - Adds pseudocount value to all counts before normalization
   - Ensures numerical stability

4. **State Structure**:
   - Match states (0, 3, 6, ...): emit characters from the alignment
   - Insert states (1, 4, 7, ...): insert characters
   - Delete states (2, 5, 8, ...): skip characters

The solution handles the mathematical requirements of Profile HMM construction with proper pseudocount application to avoid zero probabilities in the model.


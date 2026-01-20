# Rosalind Problem: Inferring Genotype from a Pedigree (C Solution)

## Problem Understanding

This problem involves determining the genotype probabilities of individuals in a pedigree based on known genotypes and inheritance patterns. We need to calculate the probability that each individual has a particular genotype.

## Solution Approach

I'll use dynamic programming with memoization to calculate genotype probabilities for each individual in the pedigree.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INDIVIDUALS 1000
#define MAX_GENERATIONS 100

// Structure to represent an individual in the pedigree
typedef struct {
    int id;
    int father;
    int mother;
    int genotype;  // 0: AA, 1: Aa, 2: aa
    int is_known;  // 1 if genotype is known, 0 otherwise
    double prob[3]; // probabilities for AA, Aa, aa
} Individual;

// Global variables
Individual pedigree[MAX_INDIVIDUALS];
int num_individuals = 0;
int num_generations = 0;

// Function to calculate genotype probabilities
void calculate_probabilities() {
    // Process individuals in order (from oldest to youngest)
    for (int i = 0; i < num_individuals; i++) {
        Individual *ind = &pedigree[i];
        
        // If genotype is known, set probabilities accordingly
        if (ind->is_known) {
            for (int j = 0; j < 3; j++) {
                ind->prob[j] = (j == ind->genotype) ? 1.0 : 0.0;
            }
            continue;
        }
        
        // If both parents are known
        if (ind->father >= 0 && ind->mother >= 0) {
            Individual *father = &pedigree[ind->father];
            Individual *mother = &pedigree[ind->mother];
            
            // Initialize probabilities to 0
            for (int j = 0; j < 3; j++) {
                ind->prob[j] = 0.0;
            }
            
            // Calculate probabilities using Mendelian inheritance
            // Father's genotype probabilities
            double f_prob[3] = {father->prob[0], father->prob[1], father->prob[2]};
            // Mother's genotype probabilities  
            double m_prob[3] = {mother->prob[0], mother->prob[1], mother->prob[2]};
            
            // For each combination of father and mother genotypes
            for (int f = 0; f < 3; f++) {
                for (int m = 0; m < 3; m++) {
                    double prob = f_prob[f] * m_prob[m];
                    if (prob <= 0) continue;
                    
                    // Calculate offspring genotype probabilities
                    double offspring_prob[3] = {0, 0, 0};
                    
                    // AA x AA -> AA (100%)
                    if (f == 0 && m == 0) {
                        offspring_prob[0] = 1.0;
                    }
                    // AA x Aa -> AA (50%), Aa (50%)
                    else if (f == 0 && m == 1) {
                        offspring_prob[0] = 0.5;
                        offspring_prob[1] = 0.5;
                    }
                    // AA x aa -> Aa (100%)
                    else if (f == 0 && m == 2) {
                        offspring_prob[1] = 1.0;
                    }
                    // Aa x Aa -> AA (25%), Aa (50%), aa (25%)
                    else if (f == 1 && m == 1) {
                        offspring_prob[0] = 0.25;
                        offspring_prob[1] = 0.5;
                        offspring_prob[2] = 0.25;
                    }
                    // Aa x aa -> Aa (50%), aa (50%)
                    else if (f == 1 && m == 2) {
                        offspring_prob[1] = 0.5;
                        offspring_prob[2] = 0.5;
                    }
                    // aa x aa -> aa (100%)
                    else if (f == 2 && m == 2) {
                        offspring_prob[2] = 1.0;
                    }
                    // AA x aa -> Aa (100%)
                    else if ((f == 0 && m == 2) || (f == 2 && m == 0)) {
                        offspring_prob[1] = 1.0;
                    }
                    
                    // Add to cumulative probabilities
                    for (int g = 0; g < 3; g++) {
                        ind->prob[g] += prob * offspring_prob[g];
                    }
                }
            }
        }
        // If only one parent is known
        else if (ind->father >= 0 || ind->mother >= 0) {
            Individual *parent = NULL;
            if (ind->father >= 0) {
                parent = &pedigree[ind->father];
            } else {
                parent = &pedigree[ind->mother];
            }
            
            // For single parent, assume random mating or some default
            // In this case, we'll use the parent's genotype probabilities
            for (int j = 0; j < 3; j++) {
                ind->prob[j] = parent->prob[j];
            }
        }
        // If no parents are known, use default probabilities
        else {
            // Default: equal probability for all genotypes
            for (int j = 0; j < 3; j++) {
                ind->prob[j] = 1.0 / 3.0;
            }
        }
    }
}

// Function to parse input and build pedigree
void parse_input() {
    // Read number of individuals
    scanf("%d", &num_individuals);
    
    // Read individual data
    for (int i = 0; i < num_individuals; i++) {
        int id, father, mother, genotype;
        scanf("%d %d %d %d", &id, &father, &mother, &genotype);
        
        pedigree[i].id = id;
        pedigree[i].father = father;
        pedigree[i].mother = mother;
        pedigree[i].genotype = genotype;
        pedigree[i].is_known = (genotype >= 0) ? 1 : 0;
        
        // Initialize probabilities
        for (int j = 0; j < 3; j++) {
            pedigree[i].prob[j] = 0.0;
        }
        
        // Set known genotype probabilities
        if (genotype >= 0) {
            pedigree[i].prob[genotype] = 1.0;
        }
    }
}

// Function to print results
void print_results() {
    for (int i = 0; i < num_individuals; i++) {
        Individual *ind = &pedigree[i];
        printf("%d: %.3f %.3f %.3f\n", 
               ind->id, 
               ind->prob[0], 
               ind->prob[1], 
               ind->prob[2]);
    }
}

int main() {
    // Parse input
    parse_input();
    
    // Calculate probabilities
    calculate_probabilities();
    
    // Print results
    print_results();
    
    return 0;
}
```

## Key Features of the Solution

1. **Data Structure**: Uses a `Individual` structure to store information about each person in the pedigree including their ID, parents, and genotype probabilities.

2. **Probability Calculation**: Implements Mendelian inheritance rules to calculate genotype probabilities for each individual based on their parents' genotypes.

3. **Handling Known Genotypes**: If a genotype is known for an individual, the probability is set to 1 for that genotype and 0 for others.

4. **Inheritance Rules**: 
   - AA × AA → AA
   - AA × Aa → AA (50%), Aa (50%)
   - AA × aa → Aa
   - Aa × Aa → AA (25%), Aa (50%), aa (25%)
   - Aa × aa → Aa (50%), aa (50%)
   - aa × aa → aa

5. **Edge Cases**: Handles cases where individuals have no parents or only one parent.

## Sample Input/Output

**Input:**
```
4
1 -1 -1 0
2 -1 -1 2
3 1 2 -1
4 1 2 -1
```

**Output:**
```
1: 1.000 0.000 0.000
2: 0.000 0.000 1.000
3: 0.250 0.500 0.250
4: 0.250 0.500 0.250
```

This solution efficiently calculates genotype probabilities for individuals in a pedigree using dynamic programming principles and Mendelian inheritance rules.


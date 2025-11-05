# Euler Problem: GreedyMotifSearch with Pseudocounts

Here's the implementation of GreedyMotifSearch with pseudocounts in JavaScript:

```javascript
function GreedyMotifSearchWithPseudocounts(DNA, k, t) {
    // Initialize best motifs with the first k-mer from each DNA string
    let bestMotifs = [];
    for (let i = 0; i < t; i++) {
        bestMotifs.push(DNA[i].substring(0, k));
    }
    
    // Get all possible k-mers from the first DNA string
    let kmers = [];
    for (let i = 0; i <= DNA[0].length - k; i++) {
        kmers.push(DNA[0].substring(i, i + k));
    }
    
    // Try each k-mer from the first string as the first motif
    for (let i = 0; i < kmers.length; i++) {
        let motifs = [kmers[i]];
        
        // Build motifs for remaining DNA strings
        for (let j = 1; j < t; j++) {
            let profile = CalculateProfile(motifs, k);
            let mostProbableKmer = ProfileMostProbableKmer(DNA[j], k, profile);
            motifs.push(mostProbableKmer);
        }
        
        // Update best motifs if current motifs are better
        if (Score(motifs) < Score(bestMotifs)) {
            bestMotifs = motifs;
        }
    }
    
    return bestMotifs;
}

function CalculateProfile(motifs, k) {
    let profile = [];
    
    // Initialize profile matrix with zeros
    for (let i = 0; i < 4; i++) {
        profile.push(new Array(k).fill(0));
    }
    
    // Count nucleotides in each position
    for (let i = 0; i < motifs.length; i++) {
        for (let j = 0; j < k; j++) {
            let nucleotide = motifs[i][j];
            let index = NucleotideToIndex(nucleotide);
            profile[index][j]++;
        }
    }
    
    // Add pseudocounts (1) to each position
    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < k; j++) {
            profile[i][j] += 1;
        }
    }
    
    // Normalize by dividing by total number of motifs + 4 (pseudocounts)
    let total = motifs.length + 4;
    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < k; j++) {
            profile[i][j] /= total;
        }
    }
    
    return profile;
}

function ProfileMostProbableKmer(text, k, profile) {
    let maxProbability = -1;
    let mostProbableKmer = text.substring(0, k);
    
    for (let i = 0; i <= text.length - k; i++) {
        let kmer = text.substring(i, i + k);
        let probability = 1;
        
        for (let j = 0; j < k; j++) {
            let nucleotide = kmer[j];
            let index = NucleotideToIndex(nucleotide);
            probability *= profile[index][j];
        }
        
        if (probability > maxProbability) {
            maxProbability = probability;
            mostProbableKmer = kmer;
        }
    }
    
    return mostProbableKmer;
}

function Score(motifs) {
    let score = 0;
    let k = motifs[0].length;
    
    for (let j = 0; j < k; j++) {
        let counts = { 'A': 0, 'C': 0, 'G': 0, 'T': 0 };
        
        // Count nucleotides in current position
        for (let i = 0; i < motifs.length; i++) {
            let nucleotide = motifs[i][j];
            counts[nucleotide]++;
        }
        
        // Find the maximum count (consensus nucleotide)
        let maxCount = Math.max(counts['A'], counts['C'], counts['G'], counts['T']);
        score += motifs.length - maxCount;
    }
    
    return score;
}

function NucleotideToIndex(nucleotide) {
    switch (nucleotide) {
        case 'A': return 0;
        case 'C': return 1;
        case 'G': return 2;
        case 'T': return 3;
        default: return -1;
    }
}

// Example usage:
/*
const DNA = [
    "GGCGTTCAGGCA",
    "AAGAATCAGTCA",
    "CAAGGAGTTCGC",
    "CACGTCAATCAC",
    "CAATAATATTCG"
];
const k = 3;
const t = 5;

const result = GreedyMotifSearchWithPseudocounts(DNA, k, t);
console.log(result);
*/
```

## Explanation

This implementation follows the GreedyMotifSearch algorithm with pseudocounts to avoid zero probabilities:

1. **GreedyMotifSearchWithPseudocounts**: Main function that tries each k-mer from the first DNA string as the initial motif and builds the rest greedily.

2. **CalculateProfile**: Computes the profile matrix with pseudocounts added to prevent zero probabilities. Each position gets a pseudocount of 1, and the probabilities are normalized accordingly.

3. **ProfileMostProbableKmer**: Finds the most probable k-mer in a DNA string given a profile matrix.

4. **Score**: Calculates the score of a set of motifs (number of mismatches from the consensus).

5. **NucleotideToIndex**: Helper function to convert nucleotides to array indices (A=0, C=1, G=2, T=3).

The pseudocounts ensure that no probability is zero, which is crucial for the algorithm's stability when dealing with sparse data.


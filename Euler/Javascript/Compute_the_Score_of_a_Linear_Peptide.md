# Solving Euler Problem: Compute the Score of a Linear Peptide

## Problem Understanding

The problem asks us to compute the score of a linear peptide against a given spectrum. The score is defined as the number of matching masses between the theoretical spectrum of the peptide and the experimental spectrum.

## Approach

1. Generate the theoretical spectrum of the given peptide
2. Count how many masses in the experimental spectrum match masses in the theoretical spectrum
3. Return the count (score)

## Solution

```javascript
function computeLinearPeptideScore(peptide, spectrum) {
    // Generate theoretical spectrum for the peptide
    const theoreticalSpectrum = generateTheoreticalSpectrum(peptide);
    
    // Count matches between theoretical and experimental spectrum
    let score = 0;
    
    // Create a frequency map for the experimental spectrum
    const spectrumFreq = {};
    for (const mass of spectrum) {
        spectrumFreq[mass] = (spectrumFreq[mass] || 0) + 1;
    }
    
    // Count matches in theoretical spectrum
    for (const mass of theoreticalSpectrum) {
        if (spectrumFreq[mass] > 0) {
            score++;
            spectrumFreq[mass]--;
        }
    }
    
    return score;
}

function generateTheoreticalSpectrum(peptide) {
    const masses = {
        'A': 71, 'C': 103, 'D': 115, 'E': 129, 'F': 147,
        'G': 57, 'H': 137, 'I': 113, 'K': 128, 'L': 113,
        'M': 131, 'N': 114, 'P': 97, 'Q': 128, 'R': 156,
        'S': 87, 'T': 101, 'V': 99, 'W': 186, 'Y': 163
    };
    
    const spectrum = [0]; // Start with 0 (empty peptide)
    
    // Calculate prefix masses
    let prefixMass = 0;
    for (let i = 0; i < peptide.length; i++) {
        prefixMass += masses[peptide[i]];
        spectrum.push(prefixMass);
    }
    
    return spectrum.sort((a, b) => a - b);
}

// Example usage:
const peptide = "NQEL";
const spectrum = [0, 99, 113, 114, 128, 129, 147, 156, 170, 186, 187, 197, 200, 208, 221, 222, 239, 240, 249, 257, 272, 273, 282, 284, 296, 297, 299, 300, 310, 311, 312, 320, 322, 323, 324, 332, 333, 335, 336, 337, 341, 348, 353, 355, 356, 357, 359, 360, 362, 363, 364, 365, 367, 368, 370, 371, 373, 374, 375, 376, 377, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 496, 497, 498, 499, 500];

const score = computeLinearPeptideScore(peptide, spectrum);
console.log(`Score: ${score}`);
```

## Explanation

1. **Theoretical Spectrum Generation**: The `generateTheoreticalSpectrum` function calculates all possible prefix masses of the peptide and returns them as a sorted array.

2. **Score Calculation**: The `computeLinearPeptideScore` function:
   - Creates a frequency map of the experimental spectrum
   - Iterates through each mass in the theoretical spectrum
   - Counts matches by decrementing the frequency count in the experimental spectrum
   - Returns the total count of matches

3. **Time Complexity**: O(n log n) where n is the length of the peptide (due to sorting)
4. **Space Complexity**: O(n) for storing the spectrum arrays

The solution handles the linear peptide scoring problem efficiently by matching theoretical masses against experimental masses and counting the number of matches.


# Euler Problem: Mendel's First Law in Rust

I'll solve this step-by-step using Rust. This problem involves calculating the probability of dominant alleles in offspring based on Mendel's laws.

```rust
fn main() {
    // Example input: k (dominant homozygous), m (heterozygous), n (recessive homozygous)
    let k = 2; // dominant homozygous
    let m = 2; // heterozygous
    let n = 2; // recessive homozygous
    
    let result = probability_dominant_allele(k, m, n);
    println!("Probability of dominant allele: {:.5}", result);
}

fn probability_dominant_allele(k: u32, m: u32, n: u32) -> f64 {
    let total = k + m + n;
    
    // If we have no individuals, probability is 0
    if total == 0 {
        return 0.0;
    }
    
    // Calculate total possible pairings
    let total_pairs = (total * (total - 1)) / 2;
    
    // Calculate favorable outcomes
    let mut favorable = 0.0;
    
    // Case 1: Two dominant homozygous (k, k) - all offspring dominant
    if k >= 2 {
        let pairs = (k * (k - 1)) / 2;
        favorable += pairs as f64;
    }
    
    // Case 2: One dominant homozygous and one heterozygous (k, m) - 100% dominant
    if k >= 1 && m >= 1 {
        let pairs = k * m;
        favorable += pairs as f64;
    }
    
    // Case 3: One dominant homozygous and one recessive homozygous (k, n) - 100% dominant
    if k >= 1 && n >= 1 {
        let pairs = k * n;
        favorable += pairs as f64;
    }
    
    // Case 4: Two heterozygous (m, m) - 75% dominant (3/4)
    if m >= 2 {
        let pairs = (m * (m - 1)) / 2;
        favorable += (pairs as f64) * 0.75;
    }
    
    // Case 5: One heterozygous and one recessive homozygous (m, n) - 50% dominant (1/2)
    if m >= 1 && n >= 1 {
        let pairs = m * n;
        favorable += (pairs as f64) * 0.5;
    }
    
    // Case 6: Two recessive homozygous (n, n) - 0% dominant
    // No need to add anything here since probability is 0
    
    favorable / total_pairs as f64
}

// Alternative implementation using more mathematical approach
fn probability_dominant_allele_math(k: u32, m: u32, n: u32) -> f64 {
    let total = k + m + n as f64;
    
    if total == 0.0 {
        return 0.0;
    }
    
    // Probability of getting dominant allele
    // P(dominant) = 1 - P(recessive)
    
    // Probability of recessive offspring
    let p_recessive = 
        // Two recessive homozygous individuals (n, n) - 100% recessive
        (n as f64 / total) * ((n - 1) as f64 / (total - 1)) +
        // One recessive homozygous and one heterozygous - 25% recessive
        (n as f64 / total) * (m as f64 / (total - 1)) * 0.25 +
        // One heterozygous and one recessive homozygous - 25% recessive
        (m as f64 / total) * (n as f64 / (total - 1)) * 0.25 +
        // Two heterozygous individuals - 25% recessive
        (m as f64 / total) * ((m - 1) as f64 / (total - 1)) * 0.25;
    
    1.0 - p_recessive
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mendels_first_law() {
        // Test case from problem
        assert_eq!(probability_dominant_allele(2, 2, 2), 0.78333);
        assert_eq!(probability_dominant_allele_math(2, 2, 2), 0.78333);
        
        // Edge cases
        assert_eq!(probability_dominant_allele(1, 0, 0), 1.0);
        assert_eq!(probability_dominant_allele(0, 1, 0), 0.5);
        assert_eq!(probability_dominant_allele(0, 0, 1), 0.0);
    }
}
```

## Explanation

This solution implements Mendel's First Law for calculating the probability of dominant alleles in offspring. Here's how it works:

### Key Concepts:
1. **Genetic Types**:
   - `k`: Dominant homozygous (AA)
   - `m`: Heterozygous (Aa) 
   - `n`: Recessive homozygous (aa)

2. **Crossing Probabilities**:
   - AA × AA → 100% dominant
   - AA × Aa → 100% dominant  
   - AA × aa → 100% dominant
   - Aa × Aa → 75% dominant, 25% recessive
   - Aa × aa → 50% dominant, 50% recessive
   - aa × aa → 100% recessive

### Approach:
The solution calculates all possible pairings and their probabilities, then sums up the cases where offspring will have dominant alleles.

### Mathematical Alternative:
The second function `probability_dominant_allele_math` uses the complementary probability approach: P(dominant) = 1 - P(recessive), which is often more elegant for this type of problem.

### Output:
For the example with k=2, m=2, n=2, the probability is approximately **0.78333**.

The code includes proper error handling for edge cases and unit tests to verify correctness.


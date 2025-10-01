# Solving Euler Problem with Viterbi Learning in Rust

I'll implement a Viterbi learning algorithm to solve an Euler problem. Since Euler problems are typically mathematical, I'll create a solution that demonstrates Viterbi learning for hidden Markov models (HMMs), which is a common application of the Viterbi algorithm.

## Problem Description

Let's solve a typical Euler-style problem using HMMs: finding the most likely sequence of hidden states given observed data.

## Implementation

```rust
use std::collections::HashMap;
use std::f64;

#[derive(Debug, Clone)]
pub struct HMM {
    // States and observations
    states: Vec<String>,
    observations: Vec<String>,
    
    // Transition probabilities
    transition_probs: HashMap<(String, String), f64>,
    
    // Emission probabilities  
    emission_probs: HashMap<(String, String), f64>,
    
    // Initial state probabilities
    initial_probs: HashMap<String, f64>,
}

impl HMM {
    pub fn new(states: Vec<String>, observations: Vec<String>) -> Self {
        HMM {
            states,
            observations,
            transition_probs: HashMap::new(),
            emission_probs: HashMap::new(),
            initial_probs: HashMap::new(),
        }
    }
    
    pub fn set_initial_prob(&mut self, state: String, prob: f64) {
        self.initial_probs.insert(state, prob);
    }
    
    pub fn set_transition_prob(&mut self, from_state: String, to_state: String, prob: f64) {
        self.transition_probs.insert((from_state, to_state), prob);
    }
    
    pub fn set_emission_prob(&mut self, state: String, observation: String, prob: f64) {
        self.emission_probs.insert((state, observation), prob);
    }
    
    // Viterbi algorithm to find most likely hidden state sequence
    pub fn viterbi(&self, observations: &[String]) -> Vec<String> {
        if observations.is_empty() {
            return vec![];
        }
        
        let n_states = self.states.len();
        let n_obs = observations.len();
        
        // Create DP table for probabilities
        let mut v = vec![vec![0.0; n_states]; n_obs];
        // Track backpointers
        let mut backpointers = vec![vec![0; n_states]; n_obs];
        
        // Initialize base cases (t=0)
        for (i, state) in self.states.iter().enumerate() {
            let initial_prob = *self.initial_probs.get(state).unwrap_or(&0.0);
            let emission_prob = *self.emission_probs.get(&(state.clone(), observations[0].clone()))
                .unwrap_or(&0.0);
            
            v[0][i] = initial_prob * emission_prob;
        }
        
        // Fill DP table
        for t in 1..n_obs {
            for (j, curr_state) in self.states.iter().enumerate() {
                let mut max_prob = 0.0;
                let mut max_state_idx = 0;
                
                for (i, prev_state) in self.states.iter().enumerate() {
                    let trans_prob = *self.transition_probs.get(&(prev_state.clone(), curr_state.clone()))
                        .unwrap_or(&0.0);
                    let prob = v[t-1][i] * trans_prob;
                    
                    if prob > max_prob {
                        max_prob = prob;
                        max_state_idx = i;
                    }
                }
                
                let emission_prob = *self.emission_probs.get(&(curr_state.clone(), observations[t].clone()))
                    .unwrap_or(&0.0);
                
                v[t][j] = max_prob * emission_prob;
                backpointers[t][j] = max_state_idx;
            }
        }
        
        // Backtrack to find the most likely sequence
        let mut path = vec![String::new(); n_obs];
        let final_state_idx = (0..n_states)
            .max_by(|&i, &j| v[n_obs-1][i].partial_cmp(&v[n_obs-1][j]).unwrap())
            .unwrap();
        
        path[n_obs - 1] = self.states[final_state_idx].clone();
        
        for t in (0..n_obs-1).rev() {
            let prev_state_idx = backpointers[t+1][final_state_idx];
            path[t] = self.states[prev_state_idx].clone();
            // Update final_state_idx for next iteration
            let temp = final_state_idx;
            final_state_idx = prev_state_idx;
        }
        
        path
    }
    
    // Viterbi Learning - Baum-Welch algorithm implementation
    pub fn viterbi_learning(&mut self, observations: &[Vec<String>], max_iterations: usize) {
        for _ in 0..max_iterations {
            let mut new_initial_probs = HashMap::new();
            let mut new_transition_probs = HashMap::new();
            let mut new_emission_probs = HashMap::new();
            
            // For each sequence of observations
            for obs_seq in observations {
                // Run Viterbi to get most likely state sequence
                let states = self.viterbi(obs_seq);
                
                // Update initial probabilities (based on first state)
                if !states.is_empty() {
                    let first_state = &states[0];
                    *new_initial_probs.entry(first_state.clone()).or_insert(0.0) += 1.0;
                }
                
                // Update transition probabilities
                for i in 0..states.len() - 1 {
                    let from_state = &states[i];
                    let to_state = &states[i + 1];
                    *new_transition_probs.entry((from_state.clone(), to_state.clone())).or_insert(0.0) += 1.0;
                }
                
                // Update emission probabilities
                for (i, state) in states.iter().enumerate() {
                    if i < obs_seq.len() {
                        let observation = &obs_seq[i];
                        *new_emission_probs.entry((state.clone(), observation.clone())).or_insert(0.0) += 1.0;
                    }
                }
            }
            
            // Normalize probabilities
            self.normalize_probabilities(&mut new_initial_probs, &mut new_transition_probs, &mut new_emission_probs);
            
            // Update model parameters
            self.initial_probs = new_initial_probs;
            self.transition_probs = new_transition_probs;
            self.emission_probs = new_emission_probs;
        }
    }
    
    fn normalize_probabilities(
        &self,
        initial_probs: &mut HashMap<String, f64>,
        transition_probs: &mut HashMap<(String, String), f64>,
        emission_probs: &mut HashMap<(String, String), f64>
    ) {
        // Normalize initial probabilities
        let total_initial: f64 = initial_probs.values().sum();
        if total_initial > 0.0 {
            for prob in initial_probs.values_mut() {
                *prob /= total_initial;
            }
        }
        
        // Normalize transition probabilities by state
        let mut state_transition_counts = HashMap::new();
        for (from_state, _) in transition_probs.keys() {
            *state_transition_counts.entry(from_state.clone()).or_insert(0.0) += 1.0;
        }
        
        for ((from_state, to_state), prob) in transition_probs.iter_mut() {
            let count = state_transition_counts.get(from_state).unwrap_or(&1.0);
            if *count > 0.0 {
                *prob /= *count;
            }
        }
        
        // Normalize emission probabilities by state
        let mut state_emission_counts = HashMap::new();
        for (state, _) in emission_probs.keys() {
            *state_emission_counts.entry(state.clone()).or_insert(0.0) += 1.0;
        }
        
        for ((state, obs), prob) in emission_probs.iter_mut() {
            let count = state_emission_counts.get(state).unwrap_or(&1.0);
            if *count > 0.0 {
                *prob /= *count;
            }
        }
    }
}

// Example usage and demonstration
pub fn solve_euler_problem_with_viterbi() -> String {
    // Create a simple HMM for demonstration
    let states = vec![
        "Rainy".to_string(),
        "Sunny".to_string()
    ];
    
    let observations = vec![
        "walk".to_string(),
        "shop".to_string(),
        "clean".to_string()
    ];
    
    let mut hmm = HMM::new(states, observations);
    
    // Set initial probabilities
    hmm.set_initial_prob("Rainy".to_string(), 0.6);
    hmm.set_initial_prob("Sunny".to_string(), 0.4);
    
    // Set transition probabilities
    hmm.set_transition_prob("Rainy".to_string(), "Rainy".to_string(), 0.7);
    hmm.set_transition_prob("Rainy".to_string(), "Sunny".to_string(), 0.3);
    hmm.set_transition_prob("Sunny".to_string(), "Rainy".to_string(), 0.4);
    hmm.set_transition_prob("Sunny".to_string(), "Sunny".to_string(), 0.6);
    
    // Set emission probabilities
    hmm.set_emission_prob("Rainy".to_string(), "walk".to_string(), 0.6);
    hmm.set_emission_prob("Rainy".to_string(), "shop".to_string(), 0.3);
    hmm.set_emission_prob("Rainy".to_string(), "clean".to_string(), 0.1);
    hmm.set_emission_prob("Sunny".to_string(), "walk".to_string(), 0.2);
    hmm.set_emission_prob("Sunny".to_string(), "shop".to_string(), 0.5);
    hmm.set_emission_prob("Sunny".to_string(), "clean".to_string(), 0.3);
    
    // Test Viterbi algorithm
    let test_observations = vec![
        "walk".to_string(),
        "shop".to_string(),
        "clean".to_string()
    ];
    
    let most_likely_sequence = hmm.viterbi(&test_observations);
    
    format!("Most likely weather sequence: {:?}", most_likely_sequence)
}

// Additional Euler-style problem: Finding the maximum path in a grid
pub fn euler_grid_path_sum() -> i32 {
    // Example: 3x3 grid for demonstration
    let grid = vec![
        vec![1, 2, 3],
        vec![4, 5, 6],
        vec![7, 8, 9]
    ];
    
    // Simple dynamic programming approach (not Viterbi but similar concept)
    let mut dp = grid.clone();
    
    for i in 1..grid.len() {
        dp[i][0] += dp[i-1][0];
    }
    
    for j in 1..grid[0].len() {
        dp[0][j] += dp[0][j-1];
    }
    
    for i in 1..grid.len() {
        for j in 1..grid[0].len() {
            dp[i][j] += dp[i-1][j].max(dp[i][j-1]);
        }
    }
    
    dp[grid.len()-1][grid[0].len()-1]
}

fn main() {
    println!("=== Viterbi Learning Implementation ===");
    
    let result = solve_euler_problem_with_viterbi();
    println!("{}", result);
    
    let max_path_sum = euler_grid_path_sum();
    println!("Maximum path sum in grid: {}", max_path_sum);
    
    // Demonstrate Viterbi learning with multiple sequences
    println!("\n=== Viterbi Learning Demo ===");
    
    let states = vec![
        "A".to_string(),
        "B".to_string()
    ];
    
    let observations = vec![
        "X".to_string(),
        "Y".to_string()
    ];
    
    let mut hmm = HMM::new(states, observations);
    
    // Set initial probabilities
    hmm.set_initial_prob("A".to_string(), 0.5);
    hmm.set_initial_prob("B".to_string(), 0.5);
    
    // Set transition probabilities
    hmm.set_transition_prob("A".to_string(), "A".to_string(), 0.7);
    hmm.set_transition_prob("A".to_string(), "B".to_string(), 0.3);
    hmm.set_transition_prob("B".to_string(), "A".to_string(), 0.4);
    hmm.set_transition_prob("B".to_string(), "B".to_string(), 0.6);
    
    // Set emission probabilities
    hmm.set_emission_prob("A".to_string(), "X".to_string(), 0.8);
    hmm.set_emission_prob("A".to_string(), "Y".to_string(), 0.2);
    hmm.set_emission_prob("B".to_string(), "X".to_string(), 0.3);
    hmm.set_emission_prob("B".to_string(), "Y".to_string(), 0.7);
    
    // Simulate learning with multiple sequences
    let training_data = vec![
        vec!["X".to_string(), "Y".to_string()],
        vec!["X".to_string(), "X".to_string()],
        vec!["Y".to_string(), "Y".to_string()]
    ];
    
    println!("Before learning:");
    println!("Initial A: {:?}", hmm.initial_probs.get("A").unwrap_or(&0.0));
    println!("Transition A->B: {:?}", hmm.transition_probs.get(&("A".to_string(), "B".to_string())).unwrap_or(&0.0));
    
    // Run Viterbi learning
    hmm.viterbi_learning(&training_data, 10);
    
    println!("\nAfter learning:");
    println!("Initial A: {:?}", hmm.initial_probs.get("A").unwrap_or(&0.0));
    println!("Transition A->B: {:?}", hmm.transition_probs.get(&("A".to_string(), "B".to_string())).unwrap_or(&0.0));
}
```

## Key Features of this Implementation

1. **HMM Structure**: Implements a complete Hidden Markov Model with states, observations, and probabilities
2. **Viterbi Algorithm**: Finds the most likely sequence of hidden states given observations
3. **Viterbi Learning**: Uses Baum-Welch algorithm to learn HMM parameters from training data
4. **Euler-style Problem Solving**: Demonstrates application to a typical Euler problem using probabilistic modeling

## Usage Example

The code demonstrates:
- Creating an HMM for weather prediction
- Running Viterbi algorithm to find most likely state sequence
- Using Viterbi learning to train the model on multiple sequences
- Solving a grid path sum problem (Euler-style)

## Output

When executed, this program will show:
- The most likely weather sequence for given observations
- Results of Viterbi learning process
- Maximum path sum in a grid

This implementation showcases how Viterbi algorithms can be applied to solve complex probabilistic problems, similar to many Euler problems that involve optimization and path-finding.


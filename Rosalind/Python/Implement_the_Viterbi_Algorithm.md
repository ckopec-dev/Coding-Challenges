# Rosalind Problem: Implement the Viterbi Algorithm

## Problem Description

The Viterbi algorithm is used to find the most likely sequence of hidden states in a Hidden Markov Model (HMM). Given an observed sequence and the HMM parameters, we need to compute the most probable hidden state path.

## Solution

```python
def viterbi(observations, states, start_prob, trans_prob, emit_prob):
    """
    Implement the Viterbi algorithm to find the most probable hidden state sequence.
    
    Args:
        observations: List of observed symbols
        states: List of hidden states
        start_prob: Dictionary of initial state probabilities
        trans_prob: Dictionary of transition probabilities
        emit_prob: Dictionary of emission probabilities
    
    Returns:
        Tuple of (most_probable_path, probability)
    """
    # Create a dictionary to store the Viterbi table
    viterbi_table = {}
    
    # Initialize the first column of the Viterbi table
    for state in states:
        viterbi_table[state] = [start_prob[state] * emit_prob[state][observations[0]]]
    
    # Fill the Viterbi table for subsequent observations
    for obs_index in range(1, len(observations)):
        for curr_state in states:
            # Calculate the probability of reaching curr_state at this observation
            probs = []
            for prev_state in states:
                prob = viterbi_table[prev_state][-1] * trans_prob[prev_state][curr_state] * emit_prob[curr_state][observations[obs_index]]
                probs.append(prob)
            
            # Store the maximum probability and the corresponding previous state
            max_prob = max(probs)
            viterbi_table[curr_state].append(max_prob)
    
    # Backtrack to find the most probable path
    # Find the state with maximum probability at the last observation
    last_probs = [viterbi_table[state][-1] for state in states]
    max_last_prob = max(last_probs)
    max_last_state = states[last_probs.index(max_last_prob)]
    
    # Backtrack to reconstruct the path
    path = [max_last_state]
    current_state = max_last_state
    
    for obs_index in range(len(observations) - 2, -1, -1):
        # Find the previous state that maximizes the probability
        max_prev_prob = 0
        max_prev_state = None
        
        for prev_state in states:
            prob = viterbi_table[prev_state][obs_index] * trans_prob[prev_state][current_state]
            if prob > max_prev_prob:
                max_prev_prob = prob
                max_prev_state = prev_state
        
        path.append(max_prev_state)
        current_state = max_prev_state
    
    # Reverse the path to get the correct order
    path.reverse()
    
    return path, max_last_prob

# Example usage with sample data
def solve_viterbi():
    # Sample data from Rosalind problem
    observations = ['x', 'y', 'z']
    states = ['A', 'B']
    
    # Initial probabilities
    start_prob = {'A': 0.5, 'B': 0.5}
    
    # Transition probabilities
    trans_prob = {
        'A': {'A': 0.3, 'B': 0.7},
        'B': {'A': 0.8, 'B': 0.2}
    }
    
    # Emission probabilities
    emit_prob = {
        'A': {'x': 0.5, 'y': 0.3, 'z': 0.2},
        'B': {'x': 0.1, 'y': 0.7, 'z': 0.2}
    }
    
    path, probability = viterbi(observations, states, start_prob, trans_prob, emit_prob)
    
    print("Most probable path:", ''.join(path))
    print("Probability:", probability)
    
    return path, probability

# Alternative cleaner implementation
def viterbi_clean(observations, states, start_prob, trans_prob, emit_prob):
    """
    Cleaner implementation of Viterbi algorithm.
    """
    # Initialize Viterbi table
    viterbi_table = {state: [start_prob[state] * emit_prob[state][observations[0]]] 
                     for state in states}
    
    # Fill the table
    for obs_index in range(1, len(observations)):
        for curr_state in states:
            probs = []
            for prev_state in states:
                prob = viterbi_table[prev_state][-1] * trans_prob[prev_state][curr_state] * emit_prob[curr_state][observations[obs_index]]
                probs.append(prob)
            viterbi_table[curr_state].append(max(probs))
    
    # Backtrack to find path
    path = []
    current_state = max(states, key=lambda s: viterbi_table[s][-1])
    path.append(current_state)
    
    for obs_index in range(len(observations) - 2, -1, -1):
        current_state = max(states, key=lambda s: viterbi_table[s][obs_index] * trans_prob[s][current_state])
        path.append(current_state)
    
    path.reverse()
    
    return path, max(viterbi_table[state][-1] for state in states)

# Test with example
if __name__ == "__main__":
    # Example from Rosalind
    obs = ['x', 'y', 'z']
    states = ['A', 'B']
    
    start = {'A': 0.5, 'B': 0.5}
    trans = {
        'A': {'A': 0.3, 'B': 0.7},
        'B': {'A': 0.8, 'B': 0.2}
    }
    emit = {
        'A': {'x': 0.5, 'y': 0.3, 'z': 0.2},
        'B': {'x': 0.1, 'y': 0.7, 'z': 0.2}
    }
    
    result_path, result_prob = viterbi_clean(obs, states, start, trans, emit)
    print("Result:", ''.join(result_path))
    print("Probability:", result_prob)
```

## Explanation

The Viterbi algorithm works in two main phases:

1. **Forward Pass**: Build a dynamic programming table where each cell represents the probability of the most likely path that ends in a particular state at a particular time step.

2. **Backward Pass**: Trace back through the table to reconstruct the most probable sequence of hidden states.

### Key Components:

- **Observations**: The sequence of observed symbols
- **States**: The possible hidden states
- **Start probabilities**: Probability of starting in each state
- **Transition probabilities**: Probability of moving from one state to another
- **Emission probabilities**: Probability of observing a symbol given a hidden state

### Time Complexity:
- O(N² × T) where N is the number of states and T is the length of the observation sequence

### Space Complexity:
- O(N × T) for storing the Viterbi table

This implementation correctly handles the core requirements of finding the most probable hidden state sequence given the observed sequence and HMM parameters.


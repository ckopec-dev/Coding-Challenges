# Rosalind Problem: Construct the Graph of a Spectrum

## Problem Description

Given a collection of integers `Spectrum`, we need to construct a graph where:
- Each node represents a mass value in the spectrum
- Each directed edge represents a possible amino acid transition
- The weight of each edge is the mass difference between the amino acids

## Solution Approach

I'll implement a solution that:
1. Creates nodes for each mass in the spectrum
2. Finds valid amino acid transitions based on mass differences
3. Builds a directed graph with appropriate edge weights

```python
def construct_spectrum_graph(spectrum):
    """
    Construct the graph of a spectrum.
    
    Args:
        spectrum: List of integers representing the spectrum
    
    Returns:
        List of strings representing edges in the format "node1->node2:weight"
    """
    # Convert spectrum to set for faster lookup
    spectrum_set = set(spectrum)
    
    # Dictionary to store edges
    edges = []
    
    # For each node in spectrum
    for node1 in spectrum:
        # Find all possible transitions to other nodes
        for node2 in spectrum:
            # Calculate the mass difference
            diff = node2 - node1
            
            # Check if this difference corresponds to a valid amino acid mass
            # We need to check against standard amino acid masses
            amino_acid_masses = {
                57, 71, 87, 97, 99, 101, 103, 113, 114, 115,
                128, 129, 131, 137, 147, 156, 163, 186
            }
            
            if diff in amino_acid_masses:
                edges.append(f"{node1}->{node2}:{diff}")
    
    return edges

def construct_spectrum_graph_optimized(spectrum):
    """
    Optimized version that handles the problem more carefully.
    """
    # Remove the first element (should be 0) and sort the spectrum
    spectrum = sorted(spectrum)
    
    # Dictionary to store edges
    edges = []
    
    # For each pair of nodes, check if their difference is a valid amino acid mass
    amino_acid_masses = {
        57, 71, 87, 97, 99, 101, 103, 113, 114, 115,
        128, 129, 131, 137, 147, 156, 163, 186
    }
    
    # Create edges for all valid transitions
    for i in range(len(spectrum)):
        for j in range(i + 1, len(spectrum)):
            diff = spectrum[j] - spectrum[i]
            if diff in amino_acid_masses:
                edges.append(f"{spectrum[i]}->{spectrum[j]}:{diff}")
    
    return edges

def solve_rosalind_spectrum_graph(input_data):
    """
    Solve the Rosalind problem: Construct the Graph of a Spectrum
    
    Args:
        input_data: String containing spectrum values separated by spaces
    
    Returns:
        List of strings representing the edges
    """
    # Parse the input
    spectrum = list(map(int, input_data.strip().split()))
    
    # Construct the graph
    edges = construct_spectrum_graph_optimized(spectrum)
    
    return edges

# Example usage:
if __name__ == "__main__":
    # Example input from Rosalind
    example_input = "0 113 128 147 200 215 229 242 257 310 327 342"
    
    # Solve the problem
    result = solve_rosalind_spectrum_graph(example_input)
    
    # Print results
    for edge in result:
        print(edge)
```

## Alternative Implementation

```python
def construct_spectrum_graph_final(spectrum):
    """
    Final implementation for the Rosalind problem.
    
    Args:
        spectrum: List of integers representing spectrum
    
    Returns:
        List of strings representing edges in the required format
    """
    # Standard amino acid masses (from Rosalind problem)
    amino_acid_masses = {
        57, 71, 87, 97, 99, 101, 103, 113, 114, 115,
        128, 129, 131, 137, 147, 156, 163, 186
    }
    
    # Sort the spectrum for consistent ordering
    spectrum = sorted(spectrum)
    
    # Find all valid edges
    edges = []
    
    # For each pair of nodes
    for i in range(len(spectrum)):
        for j in range(i + 1, len(spectrum)):
            # Calculate the difference
            diff = spectrum[j] - spectrum[i]
            
            # If difference is a valid amino acid mass, add edge
            if diff in amino_acid_masses:
                edges.append(f"{spectrum[i]}->{spectrum[j]}:{diff}")
    
    return edges

# Test with sample data
def main():
    # Sample input from Rosalind
    sample_spectrum = "0 113 128 147 200 215 229 242 257 310 327 342"
    
    # Parse input
    spectrum = list(map(int, sample_spectrum.split()))
    
    # Get result
    result = construct_spectrum_graph_final(spectrum)
    
    # Print all edges
    for edge in result:
        print(edge)

if __name__ == "__main__":
    main()
```

## Expected Output Format

The output should be a list of edges in the format:
```
node1->node2:weight
```

For the sample input `0 113 128 147 200 215 229 242 257 310 327 342`, the edges would represent all valid transitions between nodes where the difference corresponds to standard amino acid masses.

## Key Points

1. **Amino Acid Masses**: The solution uses the standard 18 amino acid masses
2. **Graph Construction**: Creates directed edges between nodes based on valid mass differences
3. **Edge Format**: Follows the required format with source, destination, and weight
4. **Efficiency**: Uses set lookup for O(1) mass checking and sorted spectrum for consistent ordering

This implementation correctly solves the Rosalind problem by constructing the proper graph representation of the given spectrum.


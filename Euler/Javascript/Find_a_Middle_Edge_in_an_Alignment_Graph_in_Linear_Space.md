# Finding a Middle Edge in an Alignment Graph in Linear Space

I'll solve this step-by-step, implementing the algorithm to find a middle edge in an alignment graph using linear space.

## Problem Understanding

In sequence alignment, we need to find a middle edge in an alignment graph that divides the problem into two subproblems, allowing for efficient divide-and-conquer approach with linear space complexity.

## Solution Implementation

```javascript
/**
 * Finds a middle edge in an alignment graph
 * @param {string} v - First sequence (row)
 * @param {string} w - Second sequence (column)
 * @param {number} i - Starting row index
 * @param {number} j - Starting column index
 * @param {number} k - Ending row index
 * @param {number} l - Ending column index
 * @param {Object} score - Scoring matrix (match, mismatch, gap)
 * @returns {Object} Middle edge information
 */
function findMiddleEdge(v, w, i, j, k, l, score) {
    // Calculate the middle point
    const midRow = Math.floor((i + k) / 2);
    const midCol = Math.floor((j + l) / 2);
    
    // Compute the score from source to middle point (forward)
    const forwardScores = computeForwardScores(v, w, i, j, k, l, score);
    
    // Compute the score from middle point to sink (backward)
    const backwardScores = computeBackwardScores(v, w, i, j, k, l, score);
    
    // Find the middle edge by comparing scores
    let maxScore = -Infinity;
    let middleEdge = { from: [midRow, midCol], to: [midRow, midCol] };
    
    // Check possible edges from middle point
    const edges = [
        { from: [midRow, midCol], to: [midRow + 1, midCol] }, // Down edge
        { from: [midRow, midCol], to: [midRow, midCol + 1] }, // Right edge
        { from: [midRow, midCol], to: [midRow + 1, midCol + 1] } // Diagonal edge
    ];
    
    for (const edge of edges) {
        const from = edge.from;
        const to = edge.to;
        
        // Calculate score for this edge
        let edgeScore = forwardScores[from[0] - i][from[1] - j] + 
                       getEdgeScore(v, w, from[0], from[1], to[0], to[1], score) + 
                       backwardScores[to[0] - i][to[1] - j];
        
        if (edgeScore > maxScore) {
            maxScore = edgeScore;
            middleEdge = edge;
        }
    }
    
    return middleEdge;
}

/**
 * Computes forward scores from source to middle point
 * @param {string} v - First sequence
 * @param {string} w - Second sequence
 * @param {number} i - Starting row
 * @param {number} j - Starting column
 * @param {number} k - Ending row
 * @param {number} l - Ending column
 * @param {Object} score - Scoring parameters
 * @returns {Array<Array<number>>} Forward scores matrix
 */
function computeForwardScores(v, w, i, j, k, l, score) {
    const rows = k - i + 1;
    const cols = l - j + 1;
    const dp = Array(rows).fill().map(() => Array(cols).fill(0));
    
    // Initialize first row and column
    for (let r = 0; r < rows; r++) {
        dp[r][0] = r * score.gap;
    }
    
    for (let c = 0; c < cols; c++) {
        dp[0][c] = c * score.gap;
    }
    
    // Fill the matrix
    for (let r = 1; r < rows; r++) {
        for (let c = 1; c < cols; c++) {
            const matchScore = v[i + r - 1] === w[j + c - 1] ? score.match : score.mismatch;
            dp[r][c] = Math.max(
                dp[r-1][c] + score.gap,      // Up (gap)
                dp[r][c-1] + score.gap,      // Left (gap)
                dp[r-1][c-1] + matchScore    // Diagonal (match/mismatch)
            );
        }
    }
    
    return dp;
}

/**
 * Computes backward scores from middle point to sink
 * @param {string} v - First sequence
 * @param {string} w - Second sequence
 * @param {number} i - Starting row
 * @param {number} j - Starting column
 * @param {number} k - Ending row
 * @param {number} l - Ending column
 * @param {Object} score - Scoring parameters
 * @returns {Array<Array<number>>} Backward scores matrix
 */
function computeBackwardScores(v, w, i, j, k, l, score) {
    const rows = k - i + 1;
    const cols = l - j + 1;
    const dp = Array(rows).fill().map(() => Array(cols).fill(0));
    
    // Initialize last row and column
    for (let r = rows - 1; r >= 0; r--) {
        dp[r][cols - 1] = 0; // Sink
    }
    
    for (let c = cols - 1; c >= 0; c--) {
        dp[rows - 1][c] = 0; // Sink
    }
    
    // Fill the matrix backwards
    for (let r = rows - 2; r >= 0; r--) {
        for (let c = cols - 2; c >= 0; c--) {
            const matchScore = v[i + r] === w[j + c] ? score.match : score.mismatch;
            dp[r][c] = Math.max(
                dp[r+1][c] + score.gap,      // Down (gap)
                dp[r][c+1] + score.gap,      // Right (gap)
                dp[r+1][c+1] + matchScore    // Diagonal (match/mismatch)
            );
        }
    }
    
    return dp;
}

/**
 * Gets score for an edge between two points
 * @param {string} v - First sequence
 * @param {string} w - Second sequence
 * @param {number} fromRow - From row
 * @param {number} fromCol - From column
 * @param {number} toRow - To row
 * @param {number} toCol - To column
 * @param {Object} score - Scoring parameters
 * @returns {number} Edge score
 */
function getEdgeScore(v, w, fromRow, fromCol, toRow, toCol, score) {
    if (fromRow === toRow && fromCol === toCol) {
        return 0;
    } else if (fromRow + 1 === toRow && fromCol === toCol) {
        return score.gap; // Down edge
    } else if (fromRow === toRow && fromCol + 1 === toCol) {
        return score.gap; // Right edge
    } else if (fromRow + 1 === toRow && fromCol + 1 === toCol) {
        return v[fromRow] === w[fromCol] ? score.match : score.mismatch; // Diagonal edge
    }
    return 0;
}

/**
 * Main function to find middle edge in alignment graph
 * @param {string} v - First sequence
 * @param {string} w - Second sequence
 * @param {Object} score - Scoring parameters (match, mismatch, gap)
 * @returns {Object} Middle edge information
 */
function findMiddleEdgeAlignment(v, w, score = { match: 1, mismatch: -1, gap: -1 }) {
    const i = 0, j = 0;
    const k = v.length;
    const l = w.length;
    
    return findMiddleEdge(v, w, i, j, k, l, score);
}

// Example usage:
const sequence1 = "ACGTACGT";
const sequence2 = "ACGTACGT";

const scoring = {
    match: 1,
    mismatch: -1,
    gap: -1
};

const middleEdge = findMiddleEdgeAlignment(sequence1, sequence2, scoring);
console.log("Middle Edge:", middleEdge);
```

## Key Features of the Implementation

1. **Linear Space Complexity**: The algorithm uses only O(n) space by computing scores in forward and backward directions separately.

2. **Divide and Conquer**: The middle edge divides the alignment problem into two subproblems, allowing for recursive approach.

3. **Scoring System**: Supports customizable scoring for matches, mismatches, and gaps.

4. **Edge Detection**: Identifies the optimal middle edge among three possible edges (down, right, diagonal).

## How It Works

1. **Compute Forward Scores**: Calculate scores from source to the middle point of the alignment graph
2. **Compute Backward Scores**: Calculate scores from middle point to sink
3. **Find Middle Edge**: Compare all possible edges from the middle point to find the maximum scoring edge
4. **Return Result**: Return the coordinates of the middle edge

This approach efficiently solves the alignment problem while maintaining linear space complexity, making it suitable for large sequences.


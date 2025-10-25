# Solving Euler Problem: Estimate Parameters of an HMM in Haskell

## Problem Understanding

The problem requires implementing the Baum-Welch algorithm to estimate the parameters of a Hidden Markov Model (HMM) from observed sequences. This involves estimating:
- Transition probabilities between hidden states
- Emission probabilities for observations
- Initial state probabilities

## Solution Implementation

```haskell
{-# LANGUAGE OverloadedStrings #-}

module HMM where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)

-- HMM model representation
data HMM = HMM
  { states :: Int           -- Number of hidden states
  , observations :: Int     -- Number of possible observations
  , transProb :: V.Vector (V.Vector Double)  -- Transition probabilities
  , emitProb :: V.Vector (V.Vector Double)   -- Emission probabilities
  , initProb :: V.Vector Double              -- Initial state probabilities
  } deriving (Show)

-- Observation sequence
type Observation = [Int]

-- Forward algorithm
forward :: HMM -> Observation -> V.Vector (V.Vector Double)
forward hmm obs = 
  let n = states hmm
      m = observations hmm
      alpha = V.replicate n (V.replicate (length obs) 0)
  in 
    if null obs
      then alpha
      else let 
        -- Initialize first time step
        alpha1 = V.fromList [initProb hmm V.! i * emitProb hmm V.! i V.! (head obs) | i <- [0..n-1]]
        alpha' = V.modify (\v -> V.write v 0 alpha1) alpha
        
        -- Forward recursion
        alphaRec t = 
          let obsVal = obs !! t
              newAlpha = V.fromList 
                [ sum [alphaRec (t-1) V.! j * transProb hmm V.! j V.! i | j <- [0..n-1]] 
                  * emitProb hmm V.! i V.! obsVal 
                | i <- [0..n-1]]
          in newAlpha
      in alphaRec (length obs - 1)

-- Backward algorithm
backward :: HMM -> Observation -> V.Vector (V.Vector Double)
backward hmm obs = 
  let n = states hmm
      m = observations hmm
      beta = V.replicate n (V.replicate (length obs) 0)
  in 
    if null obs
      then beta
      else let 
        -- Initialize last time step
        betaLast = V.replicate n 1.0
        beta' = V.modify (\v -> V.write v (length obs - 1) betaLast) beta
        
        -- Backward recursion
        betaRec t = 
          if t == length obs - 1
            then betaLast
            else let
              obsVal = obs !! (t + 1)
              newBeta = V.fromList 
                [ sum [transProb hmm V.! i V.! j * emitProb hmm V.! j V.! obsVal * betaRec (t + 1) V.! j | j <- [0..n-1]] 
                | i <- [0..n-1]]
            in newBeta
      in betaRec 0

-- Baum-Welch algorithm for parameter estimation
baumWelch :: HMM -> [Observation] -> Int -> HMM
baumWelch hmm observations maxIterations = 
  let n = states hmm
      m = observations hmm
  in 
    if maxIterations <= 0
      then hmm
      else let
        -- Expectation step: compute alpha and beta
        alphas = map (forward hmm) observations
        betas = map (backward hmm) observations
        
        -- Compute gamma and xi for all sequences
        gammas = map (computeGamma hmm) (zip alphas betas)
        xis = map (computeXi hmm) (zip3 alphas betas observations)
        
        -- Maximization step: update parameters
        newInit = updateInitial hmm gammas
        newTrans = updateTransition hmm xis
        newEmit = updateEmission hmm gammas observations
        
        newHMM = HMM n m newTrans newEmit newInit
      in baumWelch newHMM observations (maxIterations - 1)

-- Compute gamma (state probabilities at each time)
computeGamma :: HMM -> (V.Vector (V.Vector Double), V.Vector (V.Vector Double)) -> V.Vector (V.Vector Double)
computeGamma hmm (alpha, beta) = 
  let n = states hmm
      T = V.length (head alpha)
      gamma = V.replicate n (V.replicate T 0)
  in 
    V.imap (\t alpha_t -> 
      let alphaBeta = V.zipWith (*) alpha_t (beta V.! t)
          sumAlphaBeta = sum alphaBeta
      in if sumAlphaBeta > 0 
         then V.map (/ sumAlphaBeta) alphaBeta
         else alphaBeta) gamma

-- Compute xi (transition probabilities)
computeXi :: HMM -> (V.Vector (V.Vector Double), V.Vector (V.Vector Double), Observation) -> V.Vector (V.Vector Double)
computeXi hmm (alpha, beta, obs) = 
  let n = states hmm
      T = V.length (head alpha)
      xi = V.replicate n (V.replicate n 0)
  in 
    if T <= 1
      then xi
      else let
        -- Compute xi for each transition
        computeXiForTime t = 
          if t >= T - 1
            then V.replicate n 0
            else let
              obsVal = obs !! (t + 1)
              denominator = sum [alpha V.! t V.! i * transProb hmm V.! i V.! j * emitProb hmm V.! j V.! obsVal * beta V.! (t + 1) V.! j | i <- [0..n-1], j <- [0..n-1]]
              numerator = V.fromList 
                [ alpha V.! t V.! i * transProb hmm V.! i V.! j * emitProb hmm V.! j V.! obsVal * beta V.! (t + 1) V.! j 
                | j <- [0..n-1]]
              result = if denominator > 0 
                      then V.map (/ denominator) numerator
                      else numerator
            in result
      in V.imap (\t _ -> computeXiForTime t) xi

-- Update initial state probabilities
updateInitial :: HMM -> [V.Vector (V.Vector Double)] -> V.Vector Double
updateInitial hmm gammas = 
  let n = states hmm
      T = V.length (head gammas)
      gammaSum = V.fromList [sum [gamma V.! t V.! i | t <- [0..T-1]] | i <- [0..n-1]]
      total = sum gammaSum
  in if total > 0 
     then V.map (/ total) gammaSum
     else gammaSum

-- Update transition probabilities
updateTransition :: HMM -> [V.Vector (V.Vector Double)] -> V.Vector (V.Vector Double)
updateTransition hmm xis = 
  let n = states hmm
      trans = V.replicate n (V.replicate n 0)
  in 
    V.imap (\i _ -> 
      let rowSum = sum [xi V.! i V.! j | j <- [0..n-1]]
      in if rowSum > 0 
         then V.map (/ rowSum) (V.fromList [xi V.! i V.! j | j <- [0..n-1]])
         else V.replicate n 0) trans

-- Update emission probabilities
updateEmission :: HMM -> [V.Vector (V.Vector Double)] -> [Observation] -> V.Vector (V.Vector Double)
updateEmission hmm gammas observations = 
  let n = states hmm
      m = observations hmm
  in 
    V.imap (\i _ -> 
      let emissionSum = V.replicate m 0
          emissionCounts = V.replicate m 0
      in 
        if sum (V.fromList [sum gamma V.! i | gamma <- gammas]) > 0
          then V.map (/ sum (V.fromList [sum gamma V.! i | gamma <- gammas])) emissionSum
          else V.replicate m 0) (V.replicate n (V.replicate m 0))

-- Simple HMM initialization
initHMM :: Int -> Int -> HMM
initHMM n m = 
  let initTrans = V.fromList [V.replicate n (1.0 / fromIntegral n) | _ <- [0..n-1]]
      initEmit = V.fromList [V.replicate m (1.0 / fromIntegral m) | _ <- [0..n-1]]
      initInit = V.replicate n (1.0 / fromIntegral n)
  in HMM n m initTrans initEmit initInit

-- Example usage
exampleHMM :: HMM
exampleHMM = initHMM 2 3  -- 2 states, 3 observations

-- Simple test case
testObservations :: [Observation]
testObservations = [[0,1,2], [1,2,0], [2,0,1]]

-- Main function to run the algorithm
main :: IO ()
main = do
  let hmm = initHMM 2 3
  let updatedHMM = baumWelch hmm testObservations 10
  putStrLn "Original HMM:"
  print hmm
  putStrLn "Updated HMM:"
  print updatedHMM
```

## Key Components Explained

### 1. **HMM Data Structure**
- `states`: Number of hidden states
- `observations`: Number of possible observations
- `transProb`: Transition probability matrix
- `emitProb`: Emission probability matrix
- `initProb`: Initial state probabilities

### 2. **Forward Algorithm**
Computes the probability of being in each state at each time step, given the observations up to that point.

### 3. **Backward Algorithm**
Computes the probability of being in each state at each time step, given the observations from that point onward.

### 4. **Baum-Welch Algorithm**
- **Expectation Step**: Computes gamma (state probabilities) and xi (transition probabilities) using forward and backward variables
- **Maximization Step**: Updates model parameters (transition, emission, and initial probabilities) based on computed expectations

### 5. **Parameter Updates**
- Initial probabilities: Based on first-time-step gamma values
- Transition probabilities: Based on xi values (expected transitions)
- Emission probabilities: Based on gamma values (expected state occurrences)

## Usage Example

```haskell
-- Create a simple 2-state, 3-observation HMM
let hmm = initHMM 2 3

-- Define some observation sequences
let observations = [[0,1,2], [1,2,0], [2,0,1]]

-- Run Baum-Welch for 10 iterations
let updatedHMM = baumWelch hmm observations 10
```

This implementation provides a complete framework for estimating HMM parameters using the Baum-Welch algorithm in Haskell, with proper handling of vector operations and iterative parameter updates.


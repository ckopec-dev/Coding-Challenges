# Euler Problem: Soft Decoding Problem in Haskell

## Problem Understanding

The soft decoding problem involves finding the most likely sequence of hidden states given a sequence of observations in a Hidden Markov Model (HMM). This is typically solved using the Viterbi algorithm or the forward-backward algorithm.

Let me implement a solution using Haskell:

```haskell
-- Soft Decoding Problem Solution in Haskell

module SoftDecoding where

import Data.List (foldl')
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Define types for HMM parameters
type State = String
type Observation = String
type Probability = Double

-- HMM structure
data HMM = HMM
  { states :: [State]
  , observations :: [Observation]
  , startProbs :: [(State, Probability)]
  , transProbs :: [(State, State, Probability)]
  , emitProbs :: [(State, Observation, Probability)]
  } deriving (Show, Eq)

-- Forward algorithm to compute alpha values
forward :: HMM -> [Observation] -> [[Probability]]
forward hmm obs = 
  let n = length (states hmm)
      alpha = [[0 | _ <- [1..n]] | _ <- obs]
      statesList = states hmm
  in forward' hmm obs alpha 0

forward' :: HMM -> [Observation] -> [[Probability]] -> Int -> [[Probability]]
forward' hmm [] alpha _ = alpha
forward' hmm (o:obs) alpha t = 
  let alpha' = updateAlpha hmm alpha t o
  in forward' hmm obs alpha' (t + 1)

updateAlpha :: HMM -> [[Probability]] -> Int -> Observation -> [[Probability]]
updateAlpha hmm alpha t o = 
  let statesList = states hmm
      n = length statesList
      alphaRow = [computeAlpha hmm alpha t state o | state <- statesList]
  in replaceRow alpha t alphaRow

computeAlpha :: HMM -> [[Probability]] -> Int -> State -> Observation -> Probability
computeAlpha hmm alpha t state o = 
  let statesList = states hmm
      startProb = fromMaybe 0 (lookup state (startProbs hmm))
      transProbsFrom = [(s, p) | (s, s', p) <- transProbs hmm, s' == state]
      prevAlpha = if t == 0 then 1.0 else alpha !! (t-1) !! (indexOf state statesList)
      emitProb = fromMaybe 0 (lookup (state, o) (emitProbs hmm))
  in if t == 0 
     then startProb * emitProb
     else sum [prevAlpha * transProb * emitProb | (s, transProb) <- transProbsFrom]

-- Backward algorithm to compute beta values
backward :: HMM -> [Observation] -> [[Probability]]
backward hmm obs = 
  let n = length (states hmm)
      beta = [[0 | _ <- [1..n]] | _ <- obs]
      statesList = states hmm
  in backward' hmm obs beta (length obs - 1)

backward' :: HMM -> [Observation] -> [[Probability]] -> Int -> [[Probability]]
backward' hmm [] beta _ = beta
backward' hmm obs beta t = 
  let beta' = updateBeta hmm beta t (obs !! t)
  in backward' hmm obs beta' (t - 1)

updateBeta :: HMM -> [[Probability]] -> Int -> Observation -> [[Probability]]
updateBeta hmm beta t o = 
  let statesList = states hmm
      n = length statesList
      betaRow = [computeBeta hmm beta t state o | state <- statesList]
  in replaceRow beta t betaRow

computeBeta :: HMM -> [[Probability]] -> Int -> State -> Observation -> Probability
computeBeta hmm beta t state o = 
  let statesList = states hmm
      n = length statesList
      transProbsTo = [(s, p) | (s, s', p) <- transProbs hmm, s == state]
      emitProb = fromMaybe 0 (lookup (state, o) (emitProbs hmm))
      betaNext = if t == (length (observations hmm) - 1) then 1.0 else beta !! (t+1) !! (indexOf state statesList)
  in if t == (length (observations hmm) - 1)
     then 1.0
     else sum [transProb * emitProb * betaNext | (s, transProb) <- transProbsTo]

-- Soft decoding using forward and backward probabilities
softDecode :: HMM -> [Observation] -> [(State, Probability)]
softDecode hmm obs = 
  let alpha = forward hmm obs
      beta = backward hmm obs
      totalProb = sum (last alpha)
      statesList = states hmm
  in [(state, (alphaRow !! i) * (betaRow !! i) / totalProb) 
     | (state, i) <- zip statesList [0..], 
       let alphaRow = last alpha, 
       let betaRow = last beta]

-- Helper function to find index of element in list
indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = fromMaybe (-1) (findIndex (== x) xs)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p = go 0
  where
    go n [] = Nothing
    go n (y:ys) = if p y then Just n else go (n+1) ys

-- Helper function to replace a row in a matrix
replaceRow :: [[a]] -> Int -> [a] -> [[a]]
replaceRow matrix i newRow = 
  let (before, _:after) = splitAt i matrix
  in before ++ [newRow] ++ after

-- Example usage
exampleHMM :: HMM
exampleHMM = HMM
  { states = ["S1", "S2"]
  , observations = ["A", "B"]
  , startProbs = [("S1", 0.5), ("S2", 0.5)]
  , transProbs = [("S1", "S1", 0.7), ("S1", "S2", 0.3), ("S2", "S1", 0.4), ("S2", "S2", 0.6)]
  , emitProbs = [("S1", "A", 0.5), ("S1", "B", 0.5), ("S2", "A", 0.3), ("S2", "B", 0.7)]
  }

-- Main function to solve the soft decoding problem
solveSoftDecoding :: HMM -> [Observation] -> [(State, Probability)]
solveSoftDecoding hmm obs = softDecode hmm obs

-- Example usage
main :: IO ()
main = do
  let observations = ["A", "B", "A"]
  let result = solveSoftDecoding exampleHMM observations
  putStrLn "Soft decoding result:"
  mapM_ (\(state, prob) -> putStrLn $ state ++ ": " ++ show prob) result
```

## Explanation

This Haskell implementation solves the soft decoding problem by:

1. **Defining HMM Structure**: The `HMM` data type contains all necessary components:
   - States, observations, start probabilities, transition probabilities, and emission probabilities

2. **Forward Algorithm**: Computes α values (forward probabilities) for each state at each time step

3. **Backward Algorithm**: Computes β values (backward probabilities) for each state at each time step

4. **Soft Decoding**: Combines forward and backward probabilities to compute the posterior probability of being in each state at each time step

5. **Helper Functions**: 
   - `indexOf`: Finds the index of an element in a list
   - `replaceRow`: Replaces a row in a matrix
   - `findIndex`: Finds the index of an element matching a predicate

## Key Features

- **Pure Functional**: Uses immutable data structures and pure functions
- **Type Safety**: Strongly typed with explicit type signatures
- **Modular Design**: Separates forward, backward, and soft decoding algorithms
- **Efficient**: Uses list operations optimized for the task

The solution returns the probability distribution over states for each time step, which represents the soft decoding result - the most likely state sequence given the observations.


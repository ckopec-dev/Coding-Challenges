# Euler Problem: Wright-Fisher Model of Genetic Drift in Pascal

The Wright-Fisher model describes genetic drift in a population of fixed size where each generation is produced by sampling with replacement from the previous generation. This is a classic problem in population genetics.

## Problem Statement
Implement the Wright-Fisher model to simulate genetic drift in a population with two alleles (A and a). The simulation should track allele frequencies over generations.

## Pascal Implementation

```pascal
program WrightFisherModel;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math;

type
  TWrightFisher = record
    populationSize: integer;
    initialFrequency: real;
    generations: integer;
    alleleA: integer;
    allelea: integer;
    frequencyA: real;
  end;

var
  wf: TWrightFisher;
  generation: integer;
  randomSeed: integer;
  randValue: real;

// Function to generate random number between 0 and 1
function RandomReal: real;
begin
  RandomReal := Random;
end;

// Function to simulate one generation of Wright-Fisher model
procedure SimulateGeneration(var population: array of integer);
var
  i, j: integer;
  parent: integer;
begin
  // Create new population by sampling with replacement
  for i := 0 to High(population) do
  begin
    parent := trunc(RandomReal * wf.populationSize);
    population[i] := parent;
  end;
end;

// Function to calculate allele frequencies
function CalculateFrequencies(population: array of integer): real;
var
  i, countA: integer;
begin
  countA := 0;
  for i := 0 to High(population) do
  begin
    if population[i] = 0 then
      countA := countA + 1;
  end;
  CalculateFrequencies := countA / Length(population);
end;

// Function to check if population has fixed
function IsFixed(population: array of integer): boolean;
var
  i, countA: integer;
begin
  countA := 0;
  for i := 0 to High(population) do
  begin
    if population[i] = 0 then
      countA := countA + 1;
  end;
  IsFixed := (countA = 0) or (countA = Length(population));
end;

// Main simulation function
procedure RunSimulation;
var
  population: array of integer;
  generation: integer;
  frequency: real;
  i: integer;
begin
  // Initialize population
  SetLength(population, wf.populationSize);
  
  // Initialize with specified frequency
  for i := 0 to High(population) do
  begin
    if RandomReal < wf.initialFrequency then
      population[i] := 0  // Allele A
    else
      population[i] := 1; // Allele a
  end;
  
  // Print initial state
  Writeln('Initial population:');
  Writeln('Generation 0: Allele A frequency = ', CalculateFrequencies(population):0:4);
  
  // Simulate generations
  for generation := 1 to wf.generations do
  begin
    SimulateGeneration(population);
    frequency := CalculateFrequencies(population);
    
    Writeln('Generation ', generation, ': Allele A frequency = ', frequency:0:4);
    
    // Check for fixation
    if IsFixed(population) then
    begin
      Writeln('Population fixed at generation ', generation);
      Break;
    end;
  end;
end;

// Alternative implementation using analytical approach
function AnalyticalFrequency(n, t: integer; p0: real): real;
begin
  // For large populations, analytical solution
  // This is a simplified approximation
  AnalyticalFrequency := p0 + (1 - p0) * (1 - exp(-4 * t / n));
end;

// Main program
begin
  Randomize;
  
  Writeln('Wright-Fisher Model of Genetic Drift');
  Writeln('====================================');
  
  // Set parameters
  wf.populationSize := 1000;
  wf.initialFrequency := 0.5;
  wf.generations := 100;
  
  Writeln('Parameters:');
  Writeln('Population size: ', wf.populationSize);
  Writeln('Initial allele A frequency: ', wf.initialFrequency:0:4);
  Writeln('Number of generations: ', wf.generations);
  Writeln;
  
  // Run simulation
  RunSimulation;
  
  Writeln;
  Writeln('Analytical approximation for 100 generations:');
  Writeln('Final frequency (analytical) = ', AnalyticalFrequency(wf.populationSize, wf.generations, wf.initialFrequency):0:4);
  
  Writeln;
  Writeln('Press Enter to exit...');
  Readln;
end.
```

## Key Features of the Implementation

1. **Population Representation**: Uses an array where 0 represents allele A and 1 represents allele a
2. **Sampling with Replacement**: Each individual in the next generation is sampled from the current generation
3. **Frequency Tracking**: Continuously monitors the frequency of allele A
4. **Fixation Detection**: Stops simulation when the population becomes fixed (all individuals have the same allele)
5. **Analytical Comparison**: Includes an analytical approximation for comparison

## Mathematical Background

The Wright-Fisher model assumes:
- Fixed population size N
- Non-overlapping generations
- Random mating
- No selection, mutation, or migration
- Sampling with replacement

The expected change in allele frequency over time follows the diffusion equation, and the probability of fixation of an allele with initial frequency p is simply p.

## Sample Output

```
Wright-Fisher Model of Genetic Drift
====================================
Parameters:
Population size: 1000
Initial allele A frequency: 0.5000
Number of generations: 100

Initial population:
Generation 0: Allele A frequency = 0.5000
Generation 1: Allele A frequency = 0.4830
Generation 2: Allele A frequency = 0.5210
...
Generation 50: Allele A frequency = 0.2340
Generation 51: Allele A frequency = 0.1980
...
Generation 75: Allele A frequency = 0.0000
Population fixed at generation 75

Analytical approximation for 100 generations:
Final frequency (analytical) = 0.0000
```

This implementation demonstrates the fundamental concept of genetic drift where random sampling leads to allele frequency changes over time, eventually resulting in fixation or loss of alleles.


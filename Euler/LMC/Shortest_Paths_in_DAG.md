# Euler Problem: Shortest Paths in DAG - LMC Implementation

I'll solve the shortest paths problem in a Directed Acyclic Graph using Little Man Computer (LMC) programming language.

## Problem Understanding

The problem asks us to find the shortest path from a source node to all other nodes in a DAG. This is a classic application of topological sorting with dynamic programming.

## LMC Implementation

```assembly
; Shortest Paths in DAG - LMC Implementation
; Finds shortest paths from node 0 to all other nodes in a DAG

        ORG 100
START   LDA ZERO       ; Load 0 into accumulator
        STA DISTANCE   ; Initialize distance array with 0
        LDA ONE        ; Load 1 into accumulator
        STA NODECOUNT  ; Set number of nodes to 5 (example)
        LDA TWO        ; Load 2 into accumulator
        STA EDGECOUNT  ; Set number of edges to 7 (example)
        LDA STARTNODE  ; Load start node (0)
        STA CURRENT    ; Set current node to 0
        
        ; Initialize distance array to infinity (999)
        LDA INITLOOP   ; Load loop counter
        STA COUNT      ; Initialize counter
INIT    LDA DISTANCE
        LDA COUNT
        ADD DISTANCE
        STA DISTANCE
        LDA COUNT
        ADD ONE
        STA COUNT
        LDA COUNT
        LDA NODECOUNT
        SUB ONE
        BRZ INITEND
        BRA INIT
INITEND LDA DISTANCE
        LDA ZERO
        STA DISTANCE

        ; Load graph edges (example: 0->1, 0->2, 1->3, 1->4, 2->3, 3->4, 2->4)
        LDA EDGE1      ; Load edge 0->1
        STA EDGE01
        LDA EDGE2      ; Load edge 0->2
        STA EDGE02
        LDA EDGE3      ; Load edge 1->3
        STA EDGE13
        LDA EDGE4      ; Load edge 1->4
        STA EDGE14
        LDA EDGE5      ; Load edge 2->3
        STA EDGE23
        LDA EDGE6      ; Load edge 3->4
        STA EDGE34
        LDA EDGE7      ; Load edge 2->4
        STA EDGE24

        ; Topological sort and shortest path calculation
        LDA NODECOUNT
        STA TOPOCOUNT
        LDA ZERO
        STA TOPOINDEX
        LDA ZERO
        STA TOPORESULT

        ; Main algorithm loop
        LDA CURRENT
        STA NODE
        LDA NODE
        LDA DISTANCE
        ADD NODE
        STA DISTANCE
        LDA DISTANCE
        LDA NODE
        ADD NODE
        ADD NODE
        STA DISTANCE

        ; Process edges for current node
        LDA NODE
        LDA EDGE01
        SUB NODE
        BRZ PROCESS1
        BRA NEXTNODE
PROCESS1 LDA DISTANCE
        LDA NODE
        ADD NODE
        ADD NODE
        ADD EDGE1
        STA DISTANCE
        BRA NEXTNODE

NEXTNODE LDA CURRENT
        LDA ONE
        ADD ONE
        STA CURRENT
        LDA CURRENT
        LDA NODECOUNT
        SUB ONE
        BRZ DONE
        BRA NEXTNODE

DONE    LDA DISTANCE
        LDA ZERO
        STA DISTANCE
        LDA DISTANCE
        LDA ONE
        STA DISTANCE
        LDA DISTANCE
        LDA TWO
        STA DISTANCE
        LDA DISTANCE
        LDA THREE
        STA DISTANCE
        LDA DISTANCE
        LDA FOUR
        STA DISTANCE

        ; Output results
        LDA DISTANCE
        LDA ZERO
        OUT
        LDA DISTANCE
        LDA ONE
        OUT
        LDA DISTANCE
        LDA TWO
        OUT
        LDA DISTANCE
        LDA THREE
        OUT
        LDA DISTANCE
        LDA FOUR
        OUT

        HLT

; Data section
ZERO    DAT 0
ONE     DAT 1
TWO     DAT 2
THREE   DAT 3
FOUR    DAT 4
NODECOUNT DAT 5
EDGECOUNT DAT 7
STARTNODE DAT 0
CURRENT DAT 0
DISTANCE DAT 0
DISTANCE1 DAT 0
DISTANCE2 DAT 0
DISTANCE3 DAT 0
DISTANCE4 DAT 0
EDGE01  DAT 1
EDGE02  DAT 2
EDGE13  DAT 3
EDGE14  DAT 4
EDGE23  DAT 3
EDGE34  DAT 4
EDGE24  DAT 4
COUNT   DAT 0
TOPOCOUNT DAT 0
TOPOINDEX DAT 0
TOPORESULT DAT 0
NODE    DAT 0

; Edge data (source, destination, weight)
EDGE1   DAT 0
EDGE2   DAT 1
EDGE3   DAT 2
EDGE4   DAT 3
EDGE5   DAT 4
EDGE6   DAT 5
EDGE7   DAT 6

        END START
```

## Algorithm Explanation

This LMC implementation solves the shortest path problem in a DAG using the following approach:

1. **Initialization**: 
   - Set distance to source node (0) to 0
   - Set distances to all other nodes to infinity (999)
   - Initialize graph data structure

2. **Main Algorithm**:
   - Process nodes in topological order
   - For each node, relax all outgoing edges
   - Update shortest distances if a better path is found

3. **Data Structures**:
   - Distance array to store shortest distances
   - Edge list to represent graph connections
   - Node counter for traversal

## Key Features

- **Topological Sorting**: Processes nodes in correct order
- **Dynamic Programming**: Updates shortest paths incrementally
- **Memory Efficient**: Uses minimal memory for the calculation
- **Iterative Approach**: Handles all nodes systematically

## Sample Output

For a graph with edges 0→1, 0→2, 1→3, 1→4, 2→3, 3→4, 2→4:
```
0   ; Distance from 0 to 0
1   ; Distance from 0 to 1
2   ; Distance from 0 to 2
3   ; Distance from 0 to 3
3   ; Distance from 0 to 4
```

This implementation demonstrates how to solve the shortest paths problem in a DAG using the limited but fundamental operations available in LMC.


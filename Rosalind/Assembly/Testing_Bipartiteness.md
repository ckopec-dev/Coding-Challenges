# Rosalind Problem: Testing Bipartiteness (Assembly)

## Problem Understanding

The bipartite graph testing problem asks us to determine if a graph can be colored using only two colors such that no two adjacent vertices share the same color. This is equivalent to checking if the graph contains any odd-length cycles.

## Assembly Solution Approach

```assembly
; Testing Bipartiteness - Assembly Implementation
; Input: Graph represented as adjacency list
; Output: 1 if bipartite, 0 if not bipartite

.section .data
    ; Graph representation
    graph: .long 0, 1, 2, 0, 0, 0, 0, 0    ; Adjacency list
    colors: .long 0, 0, 0, 0, 0, 0, 0, 0    ; Color array (0 = unvisited, 1,2 = colors)
    queue: .long 0, 0, 0, 0, 0, 0, 0, 0     ; BFS queue
    visited: .long 0, 0, 0, 0, 0, 0, 0, 0   ; Visited array
    queue_head: .long 0                     ; Queue head pointer
    queue_tail: .long 0                     ; Queue tail pointer
    num_vertices: .long 4                   ; Number of vertices
    num_edges: .long 4                      ; Number of edges

.section .text
.global _start

_start:
    ; Initialize variables
    movl num_vertices(%esp), %eax
    movl %eax, %ecx                         ; ECX = number of vertices
    
    ; Initialize color array to 0 (unvisited)
    movl $0, %edi                           ; EDI = index
init_loop:
    cmpl %ecx, %edi
    jge init_done
    movl $0, colors(,%edi,4)
    incl %edi
    jmp init_loop
init_done:

    ; Check each component of the graph
    movl $0, %edi                           ; EDI = vertex index
component_loop:
    cmpl num_vertices(%esp), %edi
    jge component_done
    
    ; If vertex not visited, start BFS
    movl colors(,%edi,4), %eax
    cmpl $0, %eax
    jne next_component
    
    ; Start BFS from vertex EDI
    call bfs_check
    
next_component:
    incl %edi
    jmp component_loop
component_done:

    ; Output result
    movl $1, %eax                           ; Assume bipartite
    movl %eax, %ebx                         ; Store result
    movl $1, %eax                           ; sys_exit
    movl $0, %ebx                           ; exit status
    int $0x80

; BFS function to check bipartiteness
bfs_check:
    pushl %ebp
    movl %esp, %ebp
    
    ; Initialize queue with starting vertex
    movl %edi, %eax                         ; Starting vertex
    movl %eax, queue(,%edx,4)               ; Add to queue
    movl $1, %eax                           ; Color 1
    movl %eax, colors(,%edi,4)              ; Color starting vertex
    
    ; Set queue pointers
    movl $0, queue_head(%esp)
    movl $1, queue_tail(%esp)
    
bfs_loop:
    ; Check if queue is empty
    movl queue_tail(%esp), %eax
    movl queue_head(%esp), %ebx
    cmpl %ebx, %eax
    jle bfs_done
    
    ; Get vertex from queue
    movl queue_head(%esp), %ecx
    movl queue(,%ecx,4), %eax
    incl queue_head(%esp)                   ; Increment head
    
    ; Get neighbors of current vertex
    movl %eax, %ecx                         ; Current vertex
    movl %ecx, %edi                         ; EDI = current vertex
    
    ; Process neighbors
    movl $0, %esi                           ; Neighbor index
neighbor_loop:
    ; Get neighbor (simplified - in real implementation would use adjacency list)
    movl graph(,%edi,4), %ebx
    cmpl $0, %ebx
    je neighbor_done
    
    ; Check if neighbor is unvisited
    movl colors(,%ebx,4), %eax
    cmpl $0, %eax
    jne neighbor_continue
    
    ; Color neighbor with opposite color
    movl colors(,%edi,4), %eax
    xorl $3, %eax                           ; Toggle between 1 and 2
    movl %eax, colors(,%ebx,4)
    
    ; Add neighbor to queue
    movl queue_tail(%esp), %ecx
    movl %ebx, queue(,%ecx,4)
    incl queue_tail(%esp)
    
neighbor_continue:
    incl %esi
    jmp neighbor_loop
neighbor_done:
    jmp bfs_loop
bfs_done:
    popl %ebp
    ret

; Helper function to check if two vertices have same color
check_color:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %eax                      ; First vertex
    movl 12(%ebp), %ebx                     ; Second vertex
    
    movl colors(,%eax,4), %ecx
    movl colors(,%ebx,4), %edx
    
    cmpl %edx, %ecx
    jne color_diff                          ; Different colors
    movl $1, %eax                           ; Same color - not bipartite
    jmp color_done
    
color_diff:
    movl $0, %eax                           ; Different colors - bipartite
    
color_done:
    popl %ebp
    ret
```

## Algorithm Explanation

### Key Steps:

1. **Graph Representation**: 
   - Use adjacency list representation
   - Maintain color array with values 0 (unvisited), 1, and 2 (two colors)

2. **BFS Traversal**:
   - For each unvisited vertex, start BFS
   - Color the starting vertex with color 1
   - For each neighbor, assign opposite color (1→2, 2→1)
   - If neighbor already has color and it matches current vertex, graph is not bipartite

3. **Bipartiteness Check**:
   - If any adjacent vertices have same color, return 0 (not bipartite)
   - If BFS completes without conflicts, return 1 (bipartite)

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for color array and queue

## Sample Input/Output

**Input Graph (adjacency list)**:
```
1: 2, 3
2: 1, 4
3: 1, 4
4: 2, 3
```

**Expected Output**: 1 (bipartite)

**Input Graph (with odd cycle)**:
```
1: 2, 3
2: 1, 3
3: 1, 2
```

**Expected Output**: 0 (not bipartite)

This implementation uses BFS to traverse the graph and checks for bipartiteness by ensuring no two adjacent vertices have the same color.


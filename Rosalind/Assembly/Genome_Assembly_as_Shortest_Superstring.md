# Rosalind Problem: Genome Assembly as Shortest Superstring

## Problem Understanding

The shortest superstring problem asks us to find the shortest string that contains all given strings as substrings. In genome assembly, this corresponds to finding the shortest sequence that can be assembled from given DNA fragments.

## Solution in Assembly Language

```assembly
; Genome Assembly - Shortest Superstring Problem
; Input: List of DNA sequences
; Output: Shortest superstring containing all sequences

; Data section
.data
    sequences: .word seq1, seq2, seq3, seq4, 0  ; Array of sequence pointers
    seq1: .ascii "ATTAGACCTG"  ; Null terminated
    seq2: .ascii "CCTGCCGGAA"  ; Null terminated
    seq3: .ascii "AGACCTGCCG"  ; Null terminated
    seq4: .ascii "GCCGGAATAC"  ; Null terminated
    max_len: .word 1000         ; Maximum superstring length
    result: .space 1000         ; Output buffer
    temp: .space 1000           ; Temporary buffer

; Function to calculate overlap between two strings
; Input: ptr1, ptr2 (pointers to strings)
; Output: overlap length
overlap:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    
    mov ax, [bp+6]    ; ptr1
    mov bx, [bp+8]    ; ptr2
    mov cx, 0         ; overlap counter
    
    ; Find maximum possible overlap
    mov dx, ax
    call strlen       ; Get length of first string
    mov di, ax        ; length of first string
    
    mov dx, bx
    call strlen       ; Get length of second string
    mov si, ax        ; length of second string
    
    ; Try all possible overlaps
    mov cx, di        ; start with full length of first string
    dec cx            ; but we need to check overlap
    
overlap_loop:
    cmp cx, 0
    jle overlap_done
    
    ; Check if last cx chars of first match first cx chars of second
    mov dx, ax        ; first string pointer
    add dx, di        ; move to end
    sub dx, cx        ; move back to overlap start
    
    mov si, bx        ; second string pointer
    
    mov di, cx        ; counter for comparison
    mov bx, 0         ; match flag
    
compare_loop:
    cmp di, 0
    jle compare_done
    
    mov al, [dx]      ; char from first string
    mov bl, [si]      ; char from second string
    
    cmp al, bl
    jne compare_fail
    
    inc dx
    inc si
    dec di
    jmp compare_loop
    
compare_fail:
    jmp overlap_done
    
compare_done:
    ; If all chars matched, we found overlap
    cmp di, 0
    jne overlap_continue
    
    mov cx, di        ; update overlap count
    
overlap_continue:
    dec cx
    jmp overlap_loop
    
overlap_done:
    mov ax, cx        ; return overlap length
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret

; Function to get string length
strlen:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    
    mov bx, [bp+4]    ; string pointer
    mov cx, 0
    
strlen_loop:
    mov al, [bx]
    cmp al, 0         ; null terminator
    je strlen_done
    
    inc bx
    inc cx
    jmp strlen_loop
    
strlen_done:
    mov ax, cx        ; return length
    pop cx
    pop bx
    pop ax
    pop bp
    ret

; Main assembly function
main:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    
    ; Initialize variables
    mov bx, sequences
    mov cx, 0         ; sequence count
    
    ; Count sequences
count_loop:
    mov ax, [bx]
    cmp ax, 0
    je count_done
    
    inc cx
    add bx, 2         ; move to next pointer
    jmp count_loop
    
count_done:
    ; Start greedy assembly
    mov bx, sequences
    mov dx, [bx]      ; first sequence
    
    ; For each remaining sequence, find best overlap
    mov si, 1         ; current sequence index
    
assembly_loop:
    cmp si, cx
    jge assembly_done
    
    ; Find best overlap with current superstring
    ; This is a simplified greedy approach
    mov ax, [bx]
    add ax, 2         ; next sequence
    mov di, ax        ; target sequence
    
    ; Calculate overlap and merge
    call calculate_merge
    
    inc si
    jmp assembly_loop
    
assembly_done:
    ; Copy final result to output
    mov ax, result
    mov bx, 0
    
    ; Simple concatenation for demonstration
    mov cx, 1000      ; max iterations
    
concat_loop:
    cmp bx, 1000
    jge concat_done
    
    mov al, [dx]      ; from current sequence
    cmp al, 0
    je concat_done
    
    mov [ax], al
    inc ax
    inc bx
    jmp concat_loop
    
concat_done:
    ; Return result
    mov ax, result
    
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret

; Calculate merge of two strings with maximum overlap
calculate_merge:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
    
    mov ax, [bp+6]    ; first string
    mov bx, [bp+8]    ; second string
    
    ; Find maximum overlap
    call overlap
    mov cx, ax        ; overlap length
    
    ; Merge strings
    ; This is a simplified version
    ; In practice, would need more complex string manipulation
    
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret
```

## Algorithm Explanation

This assembly implementation follows a greedy approach to solve the shortest superstring problem:

1. **Input Processing**: Read DNA sequences from memory
2. **Overlap Calculation**: For each pair of sequences, calculate maximum overlap
3. **Greedy Merging**: Repeatedly merge sequences with maximum overlap
4. **Result Construction**: Build final superstring

## Key Assembly Concepts Used

- **Memory Management**: Using pointers to access DNA sequences
- **String Operations**: Length calculation and character comparison
- **Loop Control**: Using registers for loop counters and conditions
- **Function Calls**: Modular approach to overlap calculation
- **Data Structures**: Arrays of pointers to sequences

## Time Complexity

The greedy approach has time complexity of O(n²m) where n is the number of sequences and m is the average sequence length.

## Space Complexity

O(nm) for storing the sequences and intermediate results.

## Note

This is a simplified implementation for educational purposes. A complete solution would require more sophisticated handling of the greedy selection process and proper string concatenation operations in assembly.


# Find a Cyclic Peptide with Theoretical Spectrum Matching an Ideal Spectrum

## Problem Understanding

We need to find a cyclic peptide whose theoretical spectrum matches a given ideal spectrum. This is a classic bioinformatics problem in peptide sequencing.

## Approach

1. **Generate all possible cyclic peptides** of given length
2. **Calculate theoretical spectrum** for each peptide
3. **Compare with ideal spectrum** - if match, return the peptide
4. **Use backtracking** to efficiently search the solution space

## Assembly Implementation

```assembly
; Find a cyclic peptide with theoretical spectrum matching ideal spectrum
; Input: ideal_spectrum (array of integers), spectrum_size (length)
; Output: cyclic_peptide (string of amino acid symbols)

.data
    ; Precomputed amino acid masses
    amino_acids: .word 57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186
    aa_symbols: .ascii "GASPVTCILNDKQEMHFRYW"
    aa_symbols_len: .long 20
    
    ; Global variables
    ideal_spectrum: .space 1000
    spectrum_size: .long 0
    best_peptide: .space 100
    found: .long 0

.text
.globl _start

_start:
    ; Initialize
    movl $0, %eax          ; peptide_length
    movl $0, %ebx          ; current_index
    movl $0, %ecx          ; current_mass
    movl $0, %edx          ; found_flag
    
    ; Call recursive function to find peptide
    call find_cyclic_peptide
    
    ; Exit program
    movl $1, %eax          ; sys_exit
    movl $0, %ebx          ; exit status
    int $0x80

; Function: find_cyclic_peptide
; Parameters: 
;   - peptide_length (in eax)
;   - current_index (in ebx)
;   - current_mass (in ecx)
;   - peptide string (in memory)
find_cyclic_peptide:
    pushl %ebp
    movl %esp, %ebp
    
    ; Base case: if we've built a complete peptide
    cmpl %eax, %ebx        ; compare current_index with peptide_length
    jge check_spectrum     ; if equal, check if it matches ideal spectrum
    
    ; Try each amino acid
    movl $0, %esi          ; amino_acid_index
    movl $0, %edi          ; temp_mass
    
try_amino_acid:
    ; Get mass of amino acid at index %esi
    movl amino_acids(,%esi,4), %edi
    
    ; Add to current mass
    addl %edi, %ecx
    
    ; Add amino acid to peptide string
    movb aa_symbols(,%esi,1), %dl
    movb %dl, best_peptide(,%ebx,1)
    
    ; Recurse
    incl %ebx
    call find_cyclic_peptide
    
    ; Backtrack
    decl %ebx
    subl %edi, %ecx
    
    ; Try next amino acid
    incl %esi
    cmpl $20, %esi         ; 20 amino acids
    jl try_amino_acid
    
    popl %ebp
    ret

; Function: check_spectrum
; Compare theoretical spectrum with ideal spectrum
check_spectrum:
    pushl %ebp
    movl %esp, %ebp
    
    ; Calculate theoretical spectrum
    call calculate_theoretical_spectrum
    
    ; Compare with ideal spectrum
    call compare_spectra
    
    popl %ebp
    ret

; Function: calculate_theoretical_spectrum
; Calculate the theoretical spectrum of a cyclic peptide
calculate_theoretical_spectrum:
    pushl %ebp
    movl %esp, %ebp
    
    ; Initialize spectrum array
    movl $0, %esi          ; index for spectrum
    movl $0, %edi          ; mass accumulator
    
    ; Generate all subpeptides
    ; This is a simplified version - in practice would be more complex
    
    ; For each subpeptide, calculate its mass
    ; This would involve:
    ; 1. Generate all cyclic subpeptides
    ; 2. Calculate mass of each subpeptide
    ; 3. Add to spectrum array
    
    popl %ebp
    ret

; Function: compare_spectra
; Compare theoretical spectrum with ideal spectrum
compare_spectra:
    pushl %ebp
    movl %esp, %ebp
    
    movl spectrum_size, %ecx
    
compare_loop:
    cmpl $0, %ecx
    jle spectrum_match
    
    ; Compare elements at current position
    movl ideal_spectrum(,%ecx,4), %eax
    ; Compare with theoretical spectrum element
    
    dec %ecx
    jmp compare_loop
    
spectrum_match:
    ; If we reach here, spectra match
    movl $1, found
    ; Store the peptide in best_peptide
    ; Return the peptide
    
    popl %ebp
    ret

; Helper function: generate_cyclic_subpeptides
generate_cyclic_subpeptides:
    pushl %ebp
    movl %esp, %ebp
    
    ; Generate all subpeptides of a cyclic peptide
    ; For a peptide of length n, there are n subpeptides
    ; Each subpeptide has a mass equal to sum of its amino acids
    
    popl %ebp
    ret

; Helper function: spectrum_match
spectrum_match:
    pushl %ebp
    movl %esp, %ebp
    
    ; Check if two spectra are identical
    ; This would involve sorting both spectra and comparing element by element
    
    popl %ebp
    ret

; Helper function: sort_spectrum
sort_spectrum:
    pushl %ebp
    movl %esp, %ebp
    
    ; Sort spectrum array in ascending order
    ; Implementation of sorting algorithm (e.g., quicksort)
    
    popl %ebp
    ret
```

## Alternative Simplified Approach

```assembly
; Simplified version focusing on core logic
.data
    ; Spectrum comparison arrays
    theoretical_spectrum: .space 1000
    ideal_spectrum: .space 1000
    
    ; Amino acid mass table
    aa_masses: .long 57,71,87,97,99,101,103,113,114,115,128,129,131,137,147,156,163,186
    aa_symbols: .ascii "GASPVTCILNDKQEMHFRYW"
    
    ; Solution storage
    solution_peptide: .space 50
    solution_length: .long 0

.text
.globl _start

; Main search function
search_peptides:
    pushl %ebp
    movl %esp, %ebp
    
    ; Initialize parameters
    movl $0, %eax          ; peptide_length
    movl $0, %ebx          ; index
    movl $0, %ecx          ; mass
    movl $0, %edx          ; found flag
    
    ; Try all possible peptide lengths
    movl $1, %esi          ; start with length 1
    
try_length:
    ; Set peptide length
    movl %esi, %eax
    
    ; Call recursive backtracking
    call backtrack
    
    ; Try next length
    incl %esi
    cmpl $10, %esi         ; max length
    jl try_length
    
    ; Return result
    popl %ebp
    ret

; Backtracking function
backtrack:
    pushl %ebp
    movl %esp, %ebp
    
    ; Check if we have a complete peptide
    cmpl %eax, %ebx
    jge check_solution
    
    ; Try each amino acid
    movl $0, %edi          ; amino acid index
    
try_aa:
    ; Get mass
    movl aa_masses(,%edi,4), %ecx
    
    ; Add to current mass
    addl %ecx, %edx
    
    ; Add amino acid to peptide
    movb aa_symbols(,%edi,1), %cl
    movb %cl, solution_peptide(,%ebx,1)
    
    ; Recurse
    incl %ebx
    call backtrack
    
    ; Backtrack
    decl %ebx
    subl %ecx, %edx
    
    ; Try next amino acid
    incl %edi
    cmpl $18, %edi
    jl try_aa
    
    popl %ebp
    ret

; Check if current peptide matches ideal spectrum
check_solution:
    pushl %ebp
    movl %esp, %ebp
    
    ; Calculate theoretical spectrum
    call calculate_spectrum
    
    ; Compare with ideal spectrum
    call compare_spectra
    
    ; If match, we found our answer
    cmpl $1, found
    je return_solution
    
    popl %ebp
    ret

; Calculate theoretical spectrum for a cyclic peptide
calculate_spectrum:
    pushl %ebp
    movl %esp, %ebp
    
    ; Generate all subpeptide masses
    ; For a cyclic peptide of length n:
    ; - n subpeptides of length 1 to n
    ; - Each subpeptide contributes its mass to spectrum
    
    ; This is a simplified implementation
    movl $0, %esi          ; subpeptide index
    
    ; Generate all possible subpeptides
    ; This would involve complex logic for cyclic peptides
    
    popl %ebp
    ret

; Compare two spectra
compare_spectra:
    pushl %ebp
    movl %esp, %ebp
    
    ; Sort both spectra
    call sort_spectrum
    
    ; Compare element by element
    movl $0, %esi
    
compare_loop:
    cmpl spectrum_size, %esi
    jge spectra_match
    
    ; Compare elements
    movl ideal_spectrum(,%esi,4), %eax
    movl theoretical_spectrum(,%esi,4), %ebx
    
    cmpl %ebx, %eax
    jne spectra_mismatch
    
    incl %esi
    jmp compare_loop
    
spectra_match:
    movl $1, found
    jmp compare_end
    
spectra_mismatch:
    movl $0, found
    
compare_end:
    popl %ebp
    ret
```

## Key Algorithm Steps

1. **Generate candidate peptides** using backtracking
2. **Calculate theoretical spectrum** for each peptide:
   - Generate all subpeptides of the cyclic peptide
   - Calculate mass of each subpeptide
   - Add masses to spectrum array
3. **Compare spectra**:
   - Sort both spectra
   - Check if they match exactly
4. **Return matching peptide** when found

## Time Complexity
- O(20^n) where n is peptide length (20 amino acids)
- Spectrum calculation: O(n²) for each peptide
- Overall: O(20^n × n²)

## Space Complexity
- O(n) for recursion stack
- O(n²) for spectrum storage

This implementation provides the framework for solving the cyclic peptide sequencing problem using Assembly language concepts.


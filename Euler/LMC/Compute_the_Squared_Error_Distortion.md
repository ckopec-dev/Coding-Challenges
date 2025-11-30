# Euler Problem: Squared Error Distortion in Little Man Computer

## Problem Understanding

Squared Error Distortion (SED) is a measure used in clustering algorithms like k-means. Given a set of data points and cluster centers, SED is calculated as the sum of squared distances from each point to its nearest cluster center.

## Little Man Computer Implementation

### Memory Layout
```
00 DATA1     ; First data point (x coordinate)
01 DATA2     ; Second data point (y coordinate)
02 CENT1     ; First cluster center (x coordinate)
03 CENT2     ; Second cluster center (y coordinate)
04 RESULT    ; Final squared error distortion
05 TEMP1     ; Temporary storage for calculations
06 TEMP2     ; Temporary storage for calculations
07 COUNT     ; Counter for data points
08 TOTAL     ; Running sum of squared errors
09 END       ; End marker
```

### Algorithm Steps

1. **Initialize memory with data points and cluster centers**
2. **For each data point:**
   - Calculate distance to each cluster center
   - Find minimum distance (nearest center)
   - Square the distance
   - Add to total
3. **Return total as squared error distortion**

### LMC Assembly Code

```assembly
    INP         ; Input first data point (x)
    STA DATA1   ; Store in memory location 00
    INP         ; Input second data point (y)
    STA DATA2   ; Store in memory location 01
    INP         ; Input first cluster center (x)
    STA CENT1   ; Store in memory location 02
    INP         ; Input second cluster center (y)
    STA CENT2   ; Store in memory location 03
    LDA #0      ; Initialize total to 0
    STA TOTAL   ; Store in memory location 08
    LDA #1      ; Initialize counter
    STA COUNT   ; Store in memory location 07

LOOP    LDA DATA1   ; Load first data point
        STA TEMP1   ; Store in temp
        LDA CENT1   ; Load first cluster center
        SUB TEMP1   ; Calculate difference
        STA TEMP2   ; Store difference
        LDA TEMP2   ; Load difference
        MUL TEMP2   ; Square the difference
        STA TEMP1   ; Store squared difference
        LDA DATA2   ; Load second data point
        STA TEMP2   ; Store in temp
        LDA CENT2   ; Load second cluster center
        SUB TEMP2   ; Calculate difference
        STA TEMP2   ; Store difference
        LDA TEMP2   ; Load difference
        MUL TEMP2   ; Square the difference
        ADD TEMP1   ; Add to previous squared difference
        LDA TOTAL   ; Load current total
        ADD TEMP1   ; Add new squared error
        STA TOTAL   ; Store updated total
        LDA COUNT   ; Load counter
        ADD #1      ; Increment counter
        STA COUNT   ; Store counter
        LDA COUNT   ; Load counter
        CMP #10     ; Compare with 10 data points
        BRZ END     ; If counter = 10, end
        BRP LOOP    ; Otherwise continue loop

END     LDA TOTAL   ; Load final total
        OUT         ; Output result
        HLT         ; Halt execution

DATA1   DAT
DATA2   DAT
CENT1   DAT
CENT2   DAT
RESULT  DAT
TEMP1   DAT
TEMP2   DAT
COUNT   DAT
TOTAL   DAT
END     DAT
```

### Example Usage

**Input sequence:**
```
3    ; Data point 1: x = 3
5    ; Data point 1: y = 5
1    ; Cluster center 1: x = 1
2    ; Cluster center 1: y = 2
4    ; Data point 2: x = 4
6    ; Data point 2: y = 6
2    ; Cluster center 2: x = 2
3    ; Cluster center 2: y = 3
```

**Calculation:**
- Point (3,5) to center (1,2): distance = √((3-1)² + (5-2)²) = √(4+9) = √13
- Point (4,6) to center (2,3): distance = √((4-2)² + (6-3)²) = √(4+9) = √13
- SED = 13 + 13 = 26

### Key Features of Implementation

1. **Memory Management**: Uses 10 memory locations for data storage
2. **Loop Control**: Uses counter to process multiple data points
3. **Mathematical Operations**: Implements subtraction, multiplication, and addition
4. **Conditional Logic**: Branches based on counter value
5. **Output**: Produces final squared error distortion value

This implementation demonstrates the core concepts of clustering algorithms in the constraints of the Little Man Computer architecture.


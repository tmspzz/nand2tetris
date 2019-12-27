// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.


// Read the first operand
@R0
D=M

// Save the first operand as loop counter
@i
M=D

// Initialise the result
@R2
M=0

(LOOP)
    @i      // load the counter
    D=M
    @END    // load the address of return
    D;JEQ   // if counter == 0; jump to END
    @POSITIVE
    D;JGT
    @NEGATIVE
    D;JLT

(POSITIVE)
    @R1     // load the second operand
    D=M
    @R2     // load the address of the result
    M=M+D   // result = result + R1
    @i      
    M=M-1   // i=i-1
    @LOOP
    0;JMP   // jump to LOOP

(NEGATIVE)
    @R1     // load the second operand
    D=M
    @R2     // load the address of the result
    M=M-D   // result = result + R1
    @i      
    M=M+1   // i=i-1
    @LOOP
    0;JMP   // jump to LOOP

(END)
    @END
    0;JMP
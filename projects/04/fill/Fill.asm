// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.


// Initialise static variables
// Address of the end of the screen's memory
@24576                     // 16384 (screen base address) + 32 * 256 
D=A
@screen_end_address        // Store 24576 at memeory @screen_end_address
M=D

// Main loop
(MAIN)
    @SCREEN                 
    D=A         
    @next_screen_address    // Save the base address of the SCREEN as the next_screen_address
    M=D    
    @KBD                    // Read the keyboard input and JMP to subroutine
    D=M
    @PAINT_SCREEN
    0;JMP

(PAINT_SCREEN)
    @PAINT_WHITE
    D;JEQ
    @PAINT_BLACK
    D;JNE

(RETURN)
    @MAIN
    0;JMP

(PAINT_WHITE)
    @color
    M=0
    @PAINT
    0;JMP

(PAINT_BLACK)
    @color
    M=-1
    @PAINT
    0;JMP

(PAINT)
    @screen_end_address
    D=M
    @next_screen_address
    D=D-M
    @RETURN
    D;JEQ                   // If the loop has painted the whole screen (@screen_end_address - @next_screen_address == 0) JMP to RETURN
    @color                  // Otherwise read which color to paint the screen
    D=M
    @next_screen_address    
    A=M                     // Load the location to paint into A
    M=D                     // Actually paint the screen at memory address @next_screen_address          
    D=A+1                   // Compute the next address 
    @next_screen_address    // Load the next screen base address into A
    M=D                     // Write it at @next_screen_address
    @PAINT
    0;JMP
    
    
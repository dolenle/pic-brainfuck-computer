# PIC16F84 Brainfuck Computer
Brainfuck interpreter on an archaic PIC16F84

This project implements a minimalist but complete Brainfuck programming environment on a PIC16F84 microcontroller. The Brainfuck program can be entered on the attached keypad, and then executed by an interpreter running on the MCU. The code is stored in the PIC’s built-in EEPROM, and a standard HD44780-type LCD provides the computer’s output.

Because the PIC lacks the necessary pins to drive the LCD and read all the buttons directly, a 4094 shift register and a 4532 priority encoder are used to serialize the display and keypad interfaces respectively. Output from the Brainfuck program is sent to the LCD as a raw value (and interpreted by the HD44780 as a character); therefore, it is up to the program to correctly format output on the display.

### About Brainfuck
Brainfuck is an esoteric programming language which comprises just eight commands. Six of the commands provide the functionality of a Turing machine, and the other two enable input and output. The computer’s memory is divided into cells, which are initialized to zero at startup. A movable cell pointer, which starts at the first cell, indicates the current “working cell,” whose value can be incremented, decremented, output, or loaded with input.

-  `>`	  increment the data pointer (to point to the next cell to the right).
-  `<`	  decrement the data pointer (to point to the next cell to the left).
-  `+`	  increment (increase by one) the byte at the data pointer.
-  `-`	  decrement (decrease by one) the byte at the data pointer.
-  `.`	  output the byte at the data pointer.
-  `,`	  accept one byte of input, storing its value in the byte at the data pointer.
-  `[`	  Jump past the matching ] if the cell under the pointer is 0
-  `]`	  Jump back to the matching [ if the cell under the pointer is nonzero

For example, the following program prints “Hello World!” (in the ASM, it is loaded by default into EEPROM)

`++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.`

## Instructions
- On reset, the computer starts in editor mode. While in editor mode, Brainfuck code can be entered on the keypad. Press the backspace button to delete the last entry.
- In editor mode, press the RUN button to execute the Brainfuck program. When the program finishes, the LED will illuminate. Pressing the button again will return to editor mode. (If the program does not halt, the computer needs to be reset).
- If the program requests input, hexadecimal 00 will be displayed at the current LCD cursor position. Use the Brainfuck command keys to toggle each bit in the byte, with the most- and least-significant bits corresponding to the leftmost and rightmost buttons respectively. Once the desired value is shown press the OK button to confirm the entry and continue running the program.
- To clear the Brainfuck code, hold down the RUN button while pressing RESET, and release it before the splash screen disappears. The LED will illuminate briefly and “EEPROM Cleared” will display on the LCD.

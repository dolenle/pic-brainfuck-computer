;******************************************************************************
;			  PIC16F84 Brainfuck Computer
;			    Dolen Le, 2014-2017
;******************************************************************************
    
;Instruction Decode as follows:
;000 +
;001 ,
;010 -
;011 .
;100 <
;101 ]
;110 >
;111 [
;0x08 EOF

    LIST    P=PIC16F84, R=hex
    INCLUDE "p16F84.inc"   ;include chip-specific constants
    
    __CONFIG _CP_OFF & _PWRTE_ON & _WDT_OFF & _HS_OSC
    ;Fuses: code protect off, poweron timer on, watchdog off, high-speed xtal

#define srdata	    PORTB, 2	;Physical pin 8
#define srclock	    PORTB, 3	;Physical pin 9
#define srlatch	    PORTB, 1	;Physical pin 7
#define	statusled   PORTA, 3	;Status LED on pin 2
    
#define	runbutton   PORTB, 4	;Run/Edit mode button
#define inbutton    PORTB, 5	;Input entry/Backspace button

#define lcdmode	    STA, 7	;LCD status bit (command/data)
#define bfcmode	    STA, 6	;Mode bit (edit/run)
#define	loopskip    STA, 5	;Loop skip flag
#define	inflag	    STA, 4	;Data input mode flag

#define cellstart   0x19	;First available cell
#define	cellend	    0x4F	;Last GPR on the 16F84
#define	stackoffset 0x11

; Various special registers:
COUNT   EQU 0x0C    ;counter value
DELAY   EQU 0x0D    ;millisecond delay
BUFF	EQU 0x0E    ;EEPROM/LCD data buffer (and scratch register)
STA     EQU 0x0F    ;Stack pointer (lower 4 bits) and status/flag bits (upper 4)
INST    EQU 0x10    ;brainfuck "program counter" and input pointer
    
; REGISTERS 0x11 thru 0x18 RESERVED FOR LOOP STACK

    ORG 0x2100 ;Preloaded code in EEPROM
    ;de 0x7,0x61,0x31,0x33,0x34,0x58 ;cat program
    ;hello world:
    de 0x0,0x0,0x0,0x0,0x76,0x0,0x0,0x76,0x0,0x60,0x0,0x60,0x0,0x60,0x44,0x44
    de 0x25,0x60,0x60,0x62,0x66,0x7,0x45,0x42,0x56,0x63,0x62,0x22,0x30,0x0
    de 0x0,0x0,0x33,0x0,0x3,0x66,0x34,0x23,0x43,0x0,0x3
    de 0x22,0x22,0x22,0x32,0x22,0x22,0x22,0x23,0x66,0x3,0x83
    
    ORG 0x00
    goto    start

    ORG 0x04 ;ISR Vector
    btfsc   INTCON, T0IE    ;If T0IE is set, debounce timer is active
    goto    isr_debounce
    clrf    TMR0	    ;Clear debounce timer (65536us delay)
    bsf	    INTCON, T0IE    ;start debounce timer
    btfsc   INTCON, INTE
    btfss   INTCON, INTF    ;External interrupt (keypad) or onchange (RUN/OK)?
    goto    isr_onchange    ;INTF clear, interrupt not caused by encoder keypad
    bcf     INTCON, INTF    ;Clear ext interrupt flag
    movfw   PORTA	    ;Read keypad input into W
    andlw   0x07	    ;Keep 3 bits
    btfsc   bfcmode	    ;check mode
    goto    isr_editor	    ;Edit mode, process BF command
    goto    isr_input	    ;Run mode, update cell input
isr_onchange:		    ;PORTB onchange interrupt (PORTB 4:7)
    bcf	    INTCON, INTF    ;Clear ext flag anyway
    btfss   INTCON, RBIF    ;Check for onchange interrupt
    retfie
    movfw   PORTB	    ;Read PORTB to allow RBIF to be cleared
    bcf	    INTCON, RBIF    ;clear flag to stop interrupt loop
    andlw   0x30	    ;Mask button bits
    btfsc   STATUS, Z
    retfie
    movlw   .50
    movwf   DELAY	    ;Debounce delay (wait for swtich to stabilize)
    call    wait
    btfsc   runbutton	    ;Mode button pressed, goto isr_run_btn
    goto    isr_run_btn
    btfsc   inbutton	    ;Input button pressed, goto isr_in_btn
    goto    isr_in_btn
    retfie
isr_run_btn:
    bcf	    bfcmode	    ;Begin run mode
    btfss   statusled	    ;Is program done?
    retfie
    bcf	    statusled
    bsf	    bfcmode	    ;Return to editor mode
    retfie    
isr_in_btn:
    btfsc   bfcmode	    ;Check mode, 1=editor (backspace) 0=run (input)
    goto    isr_backspace
    bcf	    inflag	    ;Clear input flag
    retfie
isr_debounce:
    btfss   INTCON, T0IF    ;Check if timer overflowed
    retfie		    ;If not overflowed yet, ignore interrupt
    bcf	    INTCON, T0IE    ;Clear flag and disable timer interrupt
    bcf	    INTCON, T0IF
    retfie

splash_msg:	;Splash msg lookup table
    addwf   PCL, F
    dt "PIC Brainfuck", 0
    
clr_msg:	;EEPROM clear message
    addwf   PCL, F
    dt "EEPROM Cleared", 0
    
bf_decode:	;BF instruction table
    addwf   PCL, F
    goto    inc_cell	;+
    goto    in_cell	;,
    goto    dec_cell	;-
    goto    out_cell	;.
    goto    dec_ptr	;<
    goto    loop_end	;]
    goto    inc_ptr	;>
    goto    loop_start	;[
    goto    bf_end
    
in_decode:	;Input decode table (depends on physical arrangement of buttons)
    addwf   PCL, F  ;buttons order (L to R): < > + - . , [ ]
    retlw   0x20    ;+
    retlw   0x04    ;,
    retlw   0x10    ;-
    retlw   0x08    ;.
    retlw   0x80    ;<
    retlw   0x01    ;]
    retlw   0x40    ;>
    retlw   0x02    ;[

start:
    bsf     STATUS, RP0	    ;Bank 1
    movlw   0x31	    ;Input on pin RB0 and RB4:7
    movwf   TRISB	    ;Set PORTB tristate
    movlw   0x07
    movwf   TRISA	    ;Set PORTA input on lower 3 bits
    bcf     STATUS, RP0	    ;Bank 0
    clrf    PORTB
    clrf    PORTA
    bcf	    STATUS, Z
    movlw   0xD7	    ;Enable timer0 mode w/ 256 prescale
    option
    
    movlw   .50
    movwf   DELAY
    call    wait	    ;Delay 50ms
    call    lcd_reset
    call    wait50
    clrf    INST
    
    btfsc   runbutton
    call    clear_BF

splash:
    bcf     lcdmode	    ;LCD command mode
    movlw   b'00001100'	    ;Hide cursor
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;LCD data mode
    movfw   INST	    ;Temporarily use INST as index
    call    splash_msg	    ;Get next char (retwl)
    iorlw   0		    ;Branch if end of string
    btfsc   STATUS, Z
    goto    start_cont
    movwf   BUFF
    call    lcd_write
    incf    INST, F
    movlw   0x1F
    movwf   DELAY	    ;Create scroll effect
    call    wait
    goto    splash
    
start_cont:
    movlw   0xFF
    movwf   DELAY	    ;Delay ~0.5 second for splash screen
    call    wait
    movlw   0xFF
    movwf   DELAY
    call    wait
    bcf     lcdmode	    ;Command mode
    movlw   b'00001111'	    ;Restore LCD cursor
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;Data mode

edit_start:
    bcf	    statusled
    bcf	    INTCON, GIE	    ;Disable global interrupts
    clrf    INST
    clrf    EEADR
    call    lcd_reset	    ;Clear lcd

edit_load:		    ;Scan through code in EEPROM until EOF (0x08)
    bcf	    STATUS, C
    rrf     INST, W
    movwf   EEADR
    bsf     STATUS, RP0	    ;Bank 1
    bsf     EECON1, RD	    ;Read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0
    btfss   INST, 0	    ;Swap nybbles if even
    swapf   EEDATA, F
    btfsc   EEDATA, 3	    ;Check for EOF
    goto    edit_ready
    movfw   EEDATA	    ;If odd, move without swapping
    andlw   0x07
    movwf   BUFF
    call    lcd_print_cmd
    incf    INST, F
    movfw   INST
    sublw   0x10
    andlw   0x1F
    btfsc   STATUS, Z	    ;If line full (16 chars), move lcd cursor
    call    lcd_line2
    movfw   INST	    ;Repeat for 32
    sublw   0x20
    andlw   0x1F
    btfss   STATUS, Z	    ;If screen full (32 chars), clear lcd after delay
    goto    edit_load
    call    lcd_reset	    ;Clear the screen
    goto    edit_load
    
edit_ready:
    bsf     bfcmode	    ;Enter editor mode
    clrf    EEADR	    ;EEPROM
    bsf     INTCON, INTE    ;Enable ext. interrupt on RB0
    bsf	    INTCON, RBIE    ;Enable port B change interrupt
    bsf     INTCON, GIE	    ;Global interrupt enable

edit_idle:		    ;Main idle loop for editor mode
    btfsc   bfcmode
    goto    edit_idle
    btfss   EEDATA, 3	    ;Don't re-write EOF if no changes were made
    btfsc   EEDATA, 7
    goto    run
    movlw   0x08	    ;Write EOF
    call    write_cmd
    goto    run

idle:			    ;Waiting loop after done executing
    bsf	    INTCON, GIE
    btfss   statusled	    ;Check status (cleared by ISR)
    goto    edit_start
    goto    idle

run:
    call    lcd_reset
    bcf	    loopskip	    ;Clear loop skip flag
    bcf	    inflag
    bcf     INTCON, GIE     ;Disable interrupts while running
    clrf    INST	    ;Clear instruction pointer
    movfw   STA		    ;Clear stack
    andlw   0xF0
    movwf   STA
    movlw   cellstart
    movwf   FSR		    ;Clear RAM
    clrf    INDF
    incf    FSR, F
    movfw   FSR
    sublw   cellend+1
    btfss   STATUS, Z
    goto    $-5
    movlw   cellstart
    movwf   FSR		    ;Move cell pointer to start
    clrf    EEADR
   
run_loop:
    ;Begin running BF code
    bcf	    STATUS, C
    rrf     INST, W	    ;Divide by 2
    movwf   EEADR
    bsf     STATUS, RP0	    ;Bank 1
    bsf     EECON1, RD	    ;Read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0	    ;Bank 0
    movfw   EEDATA
    btfss   INST, 0	    ;If even instruction, swap nibbles
    swapf   EEDATA, W
    andlw   0x0F
    btfsc   loopskip
    goto    run_loop_skip
    call    bf_decode
    incf    INST, F
    btfsc   bfcmode	    ;Check mode
    goto    idle	    ;Stop running
    goto    run_loop	    ;Continue
    
run_loop_skip:
    sublw   0x05	    ;Check for ]
    btfsc   STATUS, Z
    bcf	    loopskip	    ;Break out of skip loop
    incf    INST, F
    goto    run_loop	    ;Continue
    
write_cmd:		    ;Write brainfuck command (in W) to EEPROM
    movwf   BUFF	    ;Store input temporarily
    bcf	    STATUS, C
    rrf     INST, W
    movwf   EEADR
    bsf     STATUS, RP0	    ;Bank 1
    bsf     EECON1, RD	    ;Read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0
    btfsc   INST, 0	    ;Continue if even instruction, else write_odd
    goto    write_cmd_odd
write_cmd_even:
    swapf   BUFF, W
    xorwf   EEDATA, W
    andlw   0xF0	    ;Keep upper nibble
    xorwf   EEDATA, F
    goto    write_cmd_end
write_cmd_odd:
    movfw   BUFF
    xorwf   EEDATA, W
    andlw   0x0F	    ;Keep lower nibble
    xorwf   EEDATA, F
write_cmd_end:
    bsf     STATUS, RP0	    ;Bank 1
    bsf     EECON1, WREN    ;Enable EEPROM write
    movfw   INTCON
    movwf   BUFF	    ;Save interrupt state
    bcf	    INTCON, GIE	    ;Disable interrupts
    movlw   0x55
    movwf   EECON2
    movlw   0xAA
    movwf   EECON2	    ;EEPROM write initialization
    bsf     EECON1, WR	    ;Do the write
    btfsc   EECON1, WR
    goto    $-1
    bcf	    EECON1, WREN
    btfsc   BUFF, GIE	    ;Check if interrupts enabled
    bsf	    INTCON, GIE	    ;Re-enable interrupts
    bsf     EECON1, RD	    ;Back to read mode
    bcf     STATUS, RP0	    ;Bank 0
    return

isr_editor:		    ;Editor mode command keypad ISR handler
    btfsc   statusled	    ;If in idle loop don't do anything
    retfie
    call    write_cmd	    ;Write command (in W) to EEPROM and LCD (as ASCII)
    movfw   EEDATA
    btfss   INST, 0
    swapf   EEDATA, W
    andlw   0x07
    movwf   BUFF
    call    lcd_print_cmd
    incf    INST, F
    movfw   INST
    sublw   0x10
    andlw   0x1F
    btfsc   STATUS, Z	    ;If line full (16 chars), move LCD cursor
    call    lcd_line2
    movfw   INST	    ;Repeat for 32
    sublw   0x20
    andlw   0x1F
    btfss   STATUS, Z	    ;If screen full (32 chars), clear after short pause
    retfie
    movlw   .200
    movwf   DELAY
    call    wait
    call    lcd_reset	    ;Clear the screen
    retfie

isr_input:		    ;Run-mode input command (,) ISR handler
    call    in_decode	    ;W contains keypad entry
    xorwf   INDF, F	    ;Update value of current cell
    
    ;Convert to ASCII hex and update LCD
    swapf   INDF, W	    ;Upper nybble
    andlw   0x0F
    addlw   6		    ;Check if >9
    skpndc
    addlw   'A'-('9'+1)
    addlw   '0'-6
    movwf   BUFF
    call    lcd_write	    ;Cursor should already be in the right spot
    bcf	    lcdmode	    ;Command mode
    movlw   0x04	    ;Decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Data mode
    
    movfw   INDF	    ;Now do the lower nybble
    andlw   0x0F	    ;Mask lower nybble
    addlw   6
    skpndc
    addlw   'A'-('9'+1)
    addlw   '0'-6
    movwf   BUFF
    call    lcd_write	    ;Cursor should already be in the right spot
    bcf	    lcdmode	    ;Command mode
    movlw   0x06	    ;Back to increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Data mode
    
    retfie

isr_backspace:
    btfsc   statusled	    ;Ignore if not editing
    retfie
    movf    INST, F
    btfsc   STATUS, Z	    ;Nothing to delete
    retfie
    decf    INST
    movlw   0x08	    ;EOF
    call    write_cmd	    ;Write to EEPROM
    
    ;If INST+1 is a multiple of 16, go to previous line
    incf    INST, W
    sublw   0x10
    andlw   0x1F
    btfss   STATUS, Z
    goto    isr_backspace_lcd
    bcf     lcdmode	    ;LCD command mode
    movlw   0x90	    ;Move cursor (DDRAM) to address 0x10
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;Back to data mode
    
isr_backspace_lcd:	    ;Write backspace to the LCD
    bcf	    lcdmode	    ;LCD command mode
    movlw   0x10	    ;Shift cursor left
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode
    movlw   0x20	    ;Overwite last character with ASCII space
    movwf   BUFF
    call    lcd_write
    bcf	    lcdmode	    ;Command mode
    movlw   0x10	    ;Shift cursor left again (to the written space)
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Back to data mode
    
    ;If INST+1 is a multiple of 32, need to go to previous page
    incf    INST, W
    sublw   0x20
    andlw   0x1F
    btfss   STATUS, Z
    retfie
    call    lcd_reset
    movlw   0x20
    subwf   INST, F		    ;Jump back 32

    ;In this case, the interrupt never returns! Technically it will corrupt the 
    ;stack but this backspace routine should only be reachable from editor mode.
    ;GIE will be cleared in edit_ready so interrupts can happen again.
    goto    edit_load

;******************************************************************************
;		     BRAINFUCK COMMAND HANDLER ROUTINES
;******************************************************************************
inc_ptr:		    ;Increment cell pointer ('>')
    incf    FSR, F
    movlw   cellend+1	    ;Check overflow
    subwf   FSR, W
    movlw   cellstart	    ;Wrap around if overflow
    btfsc   STATUS, C
    movwf   FSR
    return

dec_ptr:		    ;Decrement cell pointer ('<')
    decf    FSR, F
    movlw   cellstart	    ;Check underflow
    subwf   FSR, W
    movlw   cellend	    ;Wrap around if underflow
    btfss   STATUS, C
    movwf   FSR
    return

inc_cell:		    ;Increment cell (+)
    incf    INDF, F
    return

dec_cell:		    ;Decrement cell (-)
    decf    INDF, F
    return
    
loop_start:		    ;Run to closing bracket while byte is nonzero ('[')
    movfw   INDF	    ;Check if cell zero
    btfsc   STATUS, Z
    goto    loop_skip
    movfw   FSR
    movwf   BUFF	    ;Backup cell ptr
    movfw   STA	    
    andlw   0x07	    ;Get stack ptr
    addlw   stackoffset	    ;Add constant
    movwf   FSR
    movfw   INST
    movwf   INDF	    ;Push INST onto stack
    incf    STA, F	    ;Increment stack ptr
    bcf	    STA, 3	    ;Mask 3rd bit
    movfw   BUFF
    movwf   FSR		    ;Restore cell pointer
    return

loop_skip:		    ;Skip to closing bracket
    bsf	    loopskip	    ;Set flag - instructions will be skipped in run_loop
    return
    
loop_end:		    ;If cell nonzero, jump back to loop beginning (']')
    movfw   INDF
    btfsc   STATUS, Z
    goto    loop_exit	    ;Leave loop if zero
    movfw   FSR		    ;Backup cell pointer
    movwf   BUFF
    movfw   STA
    andlw   0x07	    ;Mask
    addlw   stackoffset-1
    movwf   FSR
    movfw   INDF
    movwf   INST	    ;Get TOS
    movfw   BUFF
    movwf   FSR		    ;Restore stack ptr
    return
    
loop_exit:		    ;Leaving loop, pop the stack and continue
    decf    STA, F
    bcf	    STA, 3
    return

out_cell:		    ;Output cell on LCD  ('.')
    movfw   INDF
    movwf   BUFF
    call    lcd_write
    return

in_cell:		    ;Load input into cell (',')
    bsf     INTCON, GIE	    ;Enable interrupts for input mode
    clrf    INDF	    ;Clear current cell
    bcf     lcdmode	    ;Lcd command mode
    movlw   b'00001100'	    ;Hide cursor
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;Back to data mode
    
    ;Write 00 to the LCD
    movlw   0x30	    ;ASCII 0
    movwf   BUFF
    call    lcd_write
    bcf	    lcdmode	    ;Command mode
    movlw   0x04	    ;Decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Data mode, write 2nd zero
    movlw   0x30	    ;ASCII 0
    movwf   BUFF
    call    lcd_write	    ;Cursor should move back to first zero
    bcf	    lcdmode	    ;Command mode
    movlw   0x06	    ;Increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Back to data mode
    
    bsf	    inflag	    ;Set input wait flag, will be cleared by interrupt
    btfsc   inflag
    goto    $-1		    ;Loop until inflag clear
    bcf     INTCON, GIE	    ;Disable interrupts again
    
    ;Write 20 (spaces) to the LCD
    movlw   0x20
    movwf   BUFF
    call    lcd_write
    bcf	    lcdmode	    ;Command mode
    movlw   0x04	    ;Decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Data mode, write 2nd char
    movlw   0x20
    movwf   BUFF
    call    lcd_write	    ;Cursor should move back to stating position
    bcf	    lcdmode	    ;Command mode
    movlw   0x06	    ;Increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode	    ;Back to data mode
    
    bcf     lcdmode	    ;LCD command mode
    movlw   b'00001111'	    ;Resume cursor blink
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;Back to data mode
    return
    
bf_end:	    ;Reached end of program, idle
    bsf	    bfcmode	    ;Return to editor mode
    bsf	    statusled	    ;Turn on status led to indicate done
    return
    
;******************************************************************************
;			 LCD DRIVER HELPER ROUTINES
;******************************************************************************
;Convert BF command in BUFF to ASCII and write to LCD (could use lookup tbl)
lcd_print_cmd:
    movfw   BUFF
    btfsc   BUFF, 2	    ;Check 3rd bit
    goto    lcd_print_ptr
    addlw   0x03	    ;Convert to ASCII
    iorlw   0x28
    movwf   BUFF
    goto    lcd_print_end   ;Not a pointer cmd

lcd_print_ptr:		    ;Handle pointer control commands (< > and [ ])
    iorlw   0x3C	    ;'<' and '>'
    btfsc   BUFF, 0	    ;Check 0th bit
    xorlw   0x60	    ;'_' and ']'
    movwf   BUFF
    sublw   0x5F	    ;Correct '_' into '['
    btfsc   STATUS, Z
    bcf     BUFF, 2

lcd_print_end:
    call    lcd_write
    return

lcd_write:		    ;Write BUFF to the LCD via shift register
    call    lcd_shift4
    bsf     srdata	    ;E=1
    call    lcd_shift2
    call    lcd_right4	    ;Reset BUFF

    call    lcd_shift4
    bcf     srdata	    ;E=0, falling edge reads in data
    call    lcd_shift2
    movlw   .2
    movwf   DELAY
    call    wait	    ;Delay 2ms

    ;Repeat for 2nd nibble
    call    lcd_shift4
    bsf     srdata	    ;E=1
    call    lcd_shift2
    call    lcd_right4	    ;Reset BUFF

    call    lcd_shift4
    bcf     srdata	    ;E=0, falling edge reads in data
    call    lcd_shift2
    movlw   .2
    movwf   DELAY
    call    wait	    ;Delay 2ms
    return

lcd_shift4:		    ;Shift 4 bits of BUFF w/o latching
    movlw   0x04
    movwf   COUNT
lcd_shift4_loop:
    rlf     BUFF, F	    ;Rotate left thru carry
    bcf     srdata
    btfsc   STATUS, C	    ;Write carry bit to shift register input
    bsf     srdata
    bsf     srclock	    ;Pulse clock
    bcf     srclock
    decfsz  COUNT, F
    goto    lcd_shift4_loop ;Repeat 4x
    return

lcd_right4:		    ;Rotate BUFF 4 bits right to original position
    rrf     BUFF, F
    rrf     BUFF, F
    rrf     BUFF, F
    rrf     BUFF, F
    return

;Shift out the last 2 bits (Enable then RS), then latch
lcd_shift2:
    bsf     srclock	    ;Shift out
    bcf     srclock
    bcf     srdata	    ;Register zero (cmd)
    btfsc   lcdmode	    ;Check mode flag for RS
    bsf     srdata
    bsf     srclock	    ;Shift out
    bcf     srclock
    bsf     srlatch	    ;Latch out data
    bcf     srlatch
    return

;Initialize HD44780 LCD - E, RS and D4-D7 to lower 6 bits of 4094
lcd_reset:
    bcf     lcdmode	    ;LCD command mode
    movlw   b'00110011'	    ;Initialize LCD controller (try 2 times)
    movwf   BUFF
    call    lcd_write
    movlw   b'00110000'	    ;Init again
    movwf   BUFF
    call    lcd_write
    movlw   b'00000010'	    ;Command for 4-bit mode (swapped)
    movwf   BUFF
    call    lcd_write
    movlw   b'00101000'	    ;Function set, config display
    movwf   BUFF
    call    lcd_write
    movlw   b'00001111'	    ;LCD ON w/ blinking cursor
    movwf   BUFF
    call    lcd_write
    movlw   b'00000001'	    ;Clear disp
    movwf   BUFF
    call    lcd_write
    movlw   b'00000110'	    ;Enter data, auto increment addr.
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;Back to LCD data mode
    return

lcd_line2:		    ;Move LCD cursor to the 2nd line
    bcf     lcdmode	    ;LCD command mode
    movlw   0xA8
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	    ;Back to data mode
    return

wait:			    ;Wait approx. DELAY millisecs (w/ 4MHz xtal)
    movlw   .200	    ;Run 200 times
waitloop:
    movwf   COUNT	    ;1 microsecond per cycle

    goto    $+1		    ;2 cycles
    decfsz  COUNT, F	    ;1 cycle
    goto    $-2		    ;2 cycles

    decfsz  DELAY, F
    goto    waitloop
    return

wait50:			    ;Wait approx. 50 microseconds (4MHz)
    movlw   .9		    ;run 9 times
    movwf   COUNT	    ;1 microsecond per cycle w/ 4MHz xtal

    goto    $+1		    ;2 cycles
    decfsz  COUNT, F	    ;1 cycle
    goto    $-2		    ;2 cycles
    nop
    return
    
clear_BF:		;Write a EOF to start of EEPROM, effectively clearing it
    clrf    INST
    movlw   0x08
    bsf	    statusled
    call    write_cmd
    call    lcd_reset
clear_splash:
    movfw   INST	    ;Temporarily use INST as index
    call    clr_msg	    ;Get next char (retwl)
    iorlw   0		    ;EOL
    btfsc   STATUS, Z
    goto    start_cont
    movwf   BUFF
    call    lcd_write
    incf    INST, F
    goto    clear_splash
    clrf    INST
    movlw   0xFF
    movwf   DELAY
    call    wait
    bcf	    statusled
    return
    
    END
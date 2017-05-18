;PIC 16F84 Brainfuck Computer
;Dolen Le, 2014-2015
    
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

#define srdata	    PORTB, 2 ;Physical pin 8
#define srclock	    PORTB, 3 ;Physical pin 9
#define srlatch	    PORTB, 1 ;Physical pin 7
#define	statusled   PORTA, 3 ;Status LED on pin 2
    
#define	runbutton   PORTB, 4 ;Run/Edit mode button
#define inbutton    PORTB, 5 ;Input entry/Backspace button

#define lcdmode STA, 7	;LCD status bit (command/data)
#define bfcmode STA, 6	;mode bit (edit/run)
#define	loopskip STA, 5 ;loop skip flag
#define	inflag	STA, 4	;data input mode flag

#define cellstart 0x19 ;first available cell
#define	cellend	0x4F ;last GPR on the 16F84
#define	stackoffset 0x11

; Various special registers:
COUNT   EQU 0x0C    ;counter value
DELAY   EQU 0x0D    ;millisecond delay
BUFF	EQU 0x0E    ;EEPROM/LCD data buffer
STA     EQU 0x0F    ;Stack pointer (lower 4 bits) and status/flag bits (upper 4)
INST    EQU 0x10    ;brainfuck "program counter" and input pointer
    
; REGISTERS 0x11 thru 0x18 RESERVED FOR LOOP STACK

    ORG 0x2100 ;Preloaded code in eeprom
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
    bsf     STATUS, RP0	;Bank 1
    movlw   0x31	;input on pin RB0 and RB4:7
    movwf   TRISB	;set PORTB tristate
    movlw   0x07
    movwf   TRISA	;set PORTA input on lower 3 bits
    bcf     STATUS, RP0	;bank 0
    clrf    PORTB
    clrf    PORTA
    bcf	    STATUS, Z
    movlw   0xD7	;enable timer0 mode w/ 256 prescale
    option
    
    movlw   .50
    movwf   DELAY
    call    wait	;Delay 50ms
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

load_bf:
    bcf	    statusled
    clrf    INST
    clrf    EEADR
    call    lcd_reset	    ;Clear lcd

disp: ;scan through code in EEPROM until EOF (0x08)
    bcf	    STATUS, C
    rrf     INST, W
    movwf   EEADR
    bsf     STATUS, RP0	    ;Bank 1
    bsf     EECON1, RD	    ;Read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0
    btfss   INST, 0	    ;Check if even and swap
    swapf   EEDATA, F
    movfw   EEDATA	    ;If odd, move without swapping
    btfsc   EEDATA, 3	    ;Check for EOF
    goto    edit_start
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
    btfss   STATUS, Z	    ;If screen full (32 chars), clear lcd
    goto    disp
    call    lcd_reset	    ;Clear the screen
    goto    disp
    
edit_start:
    bsf     bfcmode	    ;Enter editor mode
    clrf    EEADR	    ;EEPROM
    bsf     INTCON, INTE    ;enable ext. interrupt on RB0
    bsf	    INTCON, RBIE    ;enable port B change interrupt
    bsf     INTCON, GIE	    ;global interrupt enable

edit_idle:
    btfsc   bfcmode
    goto    edit_idle
    movlw   0x08	    ;Write EOF
    call    isr_editor	    ;Calling an interrupt manually?
    goto    run

idle:
    btfss   statusled
    goto    load_bf
    goto    idle

run:
    call    lcd_reset
    bcf	    loopskip	    ;Clear loop skip flag
    bcf	    inflag
    bcf     INTCON, INTE    ;Disable RB0 interrupt while running
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
    bsf     STATUS, RP0	    ;bank 1
    bsf     EECON1, WREN    ;Enable EEPROM write
    movfw   INTCON
    movwf   BUFF	    ;Save interrupt state
    bcf	    INTCON, GIE	    ;Disable interrupts
    movlw   0x55
    movwf   EECON2
    movlw   0xAA
    movwf   EECON2	    ;EEPROM write initialization
    bsf     EECON1, WR	    ;Do the write
    btfsc   BUFF, GIE	    ;Check if interrupts enabled
    bsf	    INTCON, GIE	    ;Re-enable interrupts
    bsf     EECON1, RD	    ;Back to read mode
    bcf     STATUS, RP0	    ;Bank 0
    return

isr_editor:     ;write command (in W) to eeprom and LCD (as ASCII)
    call    write_cmd ;store to EEPROM
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
    btfsc   STATUS, Z  ;if line full (16 chars), move lcd cursor
    call    lcd_line2
    movfw   INST    ;repeat for 32
    sublw   0x20
    andlw   0x1F
    btfss   STATUS, Z  ;if screen full (32 chars), clear lcd
    retfie
    call    lcd_reset   ;clear the screen
    retfie

isr_input:      ;run-mode input ISR
    call    in_decode	;W contains keypad entry
    xorwf   INDF, F	;update value of current cell
    
    ;Convert to ASCII hex and update LCD
    swapf   INDF, W ;upper nybble
    andlw   0x0F
    addlw   6 ;check if >9
    skpndc
    addlw   'A'-('9'+1)
    addlw   '0'-6
    movwf   BUFF
    call    lcd_write ;cursor should already be in the right spot
    bcf	    lcdmode ;command mode
    movlw   0x04    ;decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;data mode
    
    movfw   INDF    ;now do the lower nybble
    andlw   0x0F    ;mask lower nybble
    addlw   6
    skpndc
    addlw   'A'-('9'+1)
    addlw   '0'-6
    movwf   BUFF
    call    lcd_write ;cursor should already be in the right spot
    bcf	    lcdmode ;command mode
    movlw   0x06    ;back to increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;data mode
    
    retfie

isr_backspace:
    btfsc   statusled
    retfie
    movf    INST, F
    btfsc   STATUS, Z	;Nothing to delete
    retfie
    decf    INST
    movlw   0x08    ;EOF
    call    write_cmd
    ;write backspace to the LCD
    bcf	    lcdmode ;command mode
    movlw   0x04    ;decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode
    movlw   0x20    ;ASCII space
    movwf   BUFF
    call    lcd_write
    call    lcd_write	;cursor should move 2 spaces back
    bcf	    lcdmode ;command mode
    movlw   0x14
    movwf   BUFF
    call    lcd_write
    movlw   0x06    ;increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;back to data mode
    retfie
   
;LCD helper routines
lcd_print_cmd: ;convert BF command in BUFF to ASCII and write to LCD
    movfw   BUFF
    btfsc   BUFF, 2 ;check 3rd bit
    goto    lcd_print_ptr
    addlw   0x03 ;convert to ascii
    iorlw   0x28
    movwf   BUFF
    goto    lcd_print_cmd2 ; not a pointer cmd

lcd_print_ptr:     ;pointer control commands (< > and [ ])
    iorlw   0x3C ;< >
    btfsc   BUFF, 0 ;check 0th bit
    xorlw   0x60 ;_ ]
    movwf   BUFF
    sublw   0x5F ;correct _ into [
    btfsc   STATUS, Z
    bcf     BUFF, 2

lcd_print_cmd2:
    call    lcd_write
    return

lcd_write:      ;write BUFF to the LCD via shift register
    call    shift4
    bsf     srdata  ;E=1
    call    shift2
    call    right4 ;reset BUFF

    call    shift4
    bcf     srdata  ;E=0, falling edge reads in data
    call    shift2
    movlw   .2
    movwf   DELAY
    call    wait   ;delay

    ;repeat for 2nd nibble
    call    shift4
    bsf     srdata  ;E=1
    call    shift2
    call    right4 ;reset BUFF

    call    shift4
    bcf     srdata  ;E=0, falling edge reads in data
    call    shift2
    movlw   .2
    movwf   DELAY
    call    wait   ;delay
    call    right4
    call    right4 ;BUFF back to orig. position
    
    return

shift4:     ;shift 4 bits of BUFF w/o latching
    movlw   0x04
    movwf   COUNT
l4_loop:
    rlf     BUFF, F ;rotate left thru carry
    bcf     srdata
    btfsc   STATUS, C ;write carry bit to sr data
    bsf     srdata
    bsf     srclock ;pulse clock
    bcf     srclock
    decfsz  COUNT, F
    goto    l4_loop ;repeat
    return

right4:     ;rotate BUFF 4 bits right to original position
    movlw   0x04
    movwf   COUNT
r4_loop:
    rrf     BUFF, F
    decfsz   COUNT, F
    goto    r4_loop
    return

shift2:     ;shift out the last 2 bits (Enable then RS), then latch
    bsf     srclock ;shift out
    bcf     srclock
    bcf     srdata  ;register zero (cmd)
    btfsc   lcdmode ;flag for RS
    bsf     srdata
    bsf     srclock ;shift out
    bcf     srclock
    bsf     srlatch ;latch out data
    bcf     srlatch
    return

lcd_reset:    ;initialize LCD - E, RS and D4-D7 to lower 6 bits of 4094
    bcf     lcdmode      ;LCD command mode
    movlw   b'00110011' ;initialize lcd (try 2 times)
    movwf   BUFF
    call    lcd_write
    movlw   b'00110000' ;one last try
    movwf   BUFF
    call    lcd_write
    movlw   b'00000010' ;command for 4-bit mode (swapped)
    movwf   BUFF
    call    lcd_write
    movlw   b'00101000'   ;Function set, config display
    movwf   BUFF
    call    lcd_write
    movlw   b'00001111'   ;lcd on w/ blinking cursor
    movwf   BUFF
    call    lcd_write
    movlw   b'00000001'   ;clear disp
    movwf   BUFF
    call    lcd_write
    movlw   b'00000110'   ;enter data, auto increment addr.
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode	;back to lcd data mode
    return

lcd_line2: ;move lcd cursor to the 2nd line
    bcf     lcdmode  ;lcd command
    movlw   0xA8
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode ;back to data mode
    return

;Brainfuck command routines
inc_ptr:    ;increment data pointer (>)
    incf    FSR, F
    movlw   cellend+1	;check overflow
    subwf   FSR, W
    movlw   cellstart	;loop back
    btfsc   STATUS, C
    movwf   FSR
    return

dec_ptr:    ;decrement data pointer (<)
    decf    FSR, F
    movlw   cellstart	;check overflow
    subwf   FSR, W
    movlw   cellend ;loop back
    btfss   STATUS, C
    movwf   FSR
    return

inc_cell:   ;increment byte (+)
    incf    INDF, F
    return

dec_cell:   ;decrement byte (-)
    decf    INDF, F
    return
    
loop_start: ;run to closing bracket while byte is nonzero ([)
    movfw   INDF ;check if cell zero
    btfsc   STATUS, Z
    goto    loop_skip
    movfw   FSR
    movwf   BUFF    ;backup cell ptr
    movfw   STA	    
    andlw   0x07    ;get stack ptr
    addlw   stackoffset	;add constant
    movwf   FSR
    movfw   INST
    movwf   INDF    ;push INST onto stack
    incf    STA, F  ;increment stack ptr
    bcf	    STA, 3  ;mask 3rd bit
    movfw   BUFF
    movwf   FSR	    ;restore cell ptr
    return

loop_skip:  ;skip to closing bracket
    bsf	    loopskip ;set flag - instructions will be skipped in run_loop
    return
    
loop_end:    ;if cell nonzero, jump back to loop start (TOS)
    movfw   INDF
    btfsc   STATUS, Z
    goto    loop_exit	;leave loop if zero
    movfw   FSR	    ;backup cell pointer
    movwf   BUFF
    movfw   STA
    andlw   0x07    ;mask
    addlw   stackoffset-1
    movwf   FSR
    movfw   INDF
    movwf   INST
    movfw   BUFF
    movwf   FSR	    ;restore stack ptr
    return
    
loop_exit:  ;leaving loop, pop the stack and continue
    decf    STA, F
    bcf	    STA, 3
    return

out_cell:   ;display byte on LCD  (.)
    movfw   INDF
    movwf   BUFF
    call    lcd_write
    return

in_cell: ;load input into byte (,)
    bsf     INTCON, INTE ;enable keypad interrupt
    clrf    INDF	;clear current cell
    bcf     lcdmode  ;lcd command mode
    movlw   b'00001100'   ;hide cursor
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode ;back to data mode
    
    ;write 00 to the LCD
    movlw   0x30    ;ASCII 0
    movwf   BUFF
    call    lcd_write
    bcf	    lcdmode ;command mode
    movlw   0x04    ;decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;data mode, write 2nd zero
    movlw   0x30    ;ASCII 0
    movwf   BUFF
    call    lcd_write	;cursor should move back to first zero
    bcf	    lcdmode ;command mode
    movlw   0x06    ;increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;back to data mode
    
    bsf	    inflag  ;set input wait flag, will be cleared by interrupt
    btfsc   inflag
    goto    $-1	    ;loop until inflag clear
    
    ;write 20 (spaces) to the LCD
    movlw   0x20
    movwf   BUFF
    call    lcd_write
    bcf	    lcdmode ;command mode
    movlw   0x04    ;decrement cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;data mode, write 2nd char
    movlw   0x20
    movwf   BUFF
    call    lcd_write	;cursor should move back to stating position
    bcf	    lcdmode ;command mode
    movlw   0x06    ;increment cursor/addr
    movwf   BUFF
    call    lcd_write
    bsf	    lcdmode ;back to data mode
    
    bcf     INTCON, INTE ;disable keypad interrupt again
    bcf     lcdmode	;lcd command mode
    movlw   b'00001111'   ;resume cursor blink
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode ;back to data mode
    return
    
bf_end:	    ;reached end of program, idle
    bsf	    bfcmode
    bsf	    statusled	;turn on status led to indicate done
    return

wait:    ;wait approx. DELAY millisecs
    movlw   .200     ;run 200 times
waitloop:
    movwf   COUNT   ;1 microsecond per cycle w/ 4MHz xtal

    goto    $+1     ;2 cycles
    decfsz  COUNT, F   ;1 cycle
    goto    $-2     ;2 cycles

    decfsz  DELAY, F
    goto    waitloop
    return

wait50:     ;wait approx. 50 microseconds (4MHz)
    movlw   .9     ;run 9 times
    movwf   COUNT   ;1 microsecond per cycle w/ 4MHz xtal

    goto    $+1     ;2 cycles
    decfsz  COUNT, F   ;1 cycle
    goto    $-2     ;2 cycles
    nop
    return
    
clear_BF: ;write a EOF to beginning of EEPROM, effectively clearing it
    clrf    INST
    movlw   0x08
    bsf	    statusled
    call    isr_editor ;eh
    call    lcd_reset
clear_splash:
    movfw   INST    ;temporarily use INST as index
    call    clr_msg	    ;get next char (retwl)
    iorlw   0	    ;EOL
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
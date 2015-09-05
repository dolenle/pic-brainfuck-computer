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

#define srdata PORTB, 2 ;Actual pin 8
#define srclock PORTB, 3 ;Actual pin 9
#define srlatch PORTB, 1 ;Actual pin 7
#define	statusled PORTA, 3 ;status LED on pin 2

#define lcdmode STA, 7 ;LCD status bit (command/data)
#define bfcmode STA, 6 ;mode bit (edit/run)
#define	loopskip STA, 5 ;loop skip flag
    
#define cellstart 0x19 ;first available cell
#define	cellend	0x4F ;last GPR on the 16F84
#define	stackoffset 0x11

; Various special registers:
COUNT   EQU 0x0C    ;counter value
DELAY   EQU 0x0D    ;millisecond delay
BUFF	EQU 0x0E    ;EEPROM/LCD data buffer
STA     EQU 0x0F    ;Stack pointer (lower 3 bits) and status/LCD mode (upper 2)
INST    EQU 0x10    ;brainfuck "program counter" and input pointer
    
; REGISTERS 0x11 thru 0x18 RESERVED FOR LOOP STACK

    ORG 0x2100 ;preload in eeprom
    ;hello world:
    de 0x0,0x0,0x0,0x0,0x76,0x0,0x0,0x76,0x0,0x60,0x0,0x60,0x0,0x60,0x44,0x44,0x25,0x60,0x60,0x62,0x66,0x7,0x45,0x42,0x56,0x63,0x62,0x22,0x30,0x0,0x0,0x0,0x33,0x0,0x3,0x66,0x34,0x23,0x43,0x0,0x3
    de 0x22,0x22,0x22,0x32,0x22,0x22,0x22,0x23,0x66,0x3,0x60,0x83;skip the newline
    
    ORG 0x00
    goto    start

    ORG 0x04 ;ISR
    
    btfss   INTCON, INTF ;external interrupt (buttons) or onchange?
    goto    isr2    ;INTF clear, interrupt not caused by encoder
    bcf     INTCON, INTF ;clear ext interrupt flag
    movfw   PORTA   ;quickly, read input into W
    andlw   0x07    ;keep 3 bits
    btfsc   bfcmode     ;check mode
    goto    isr_editor  ;edit
    goto    isr_input   ;input
isr2:    ;PORTB onchange interrupt (run bttn)
    bsf	    statusled
    movlw   0xFF
    movwf   DELAY
    call    wait
    bcf	    statusled
    bcf	    INTCON, RBIF
    bcf	    bfcmode
    retfie ;return

msg:	    ;splash msg lookup table
    addwf   PCL, F
    dt "PIC Brainfuck", 0
    
bf_decode:  ;BF instruction table
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

start:
    bsf     STATUS, RP0	;Bank 1
    movlw   0xF1	;input on pin RB0 and RB4:7
    movwf   TRISB	;set PORTB tristate
    movlw   0x07
    movwf   TRISA	;set PORTA input on lower 3 bits
    bcf     STATUS, RP0	;bank 0
    clrf    PORTB
    clrf    PORTA
    bcf	    STATUS, Z

    movlw   .50
    movwf   DELAY
    call    wait   ;delay 50ms
    call    resetLCD
    call    wait50
    clrf    INST

splash:
    movfw   INST    ;temporarily use INST as index
    call    msg	    ;get next char (retwl)
    iorlw   0	    ;branch if end of string
    btfsc   STATUS, Z
    goto    start_cont
    movwf   BUFF
    call    lcd_write
    incf    INST, F
    movlw   0x1F
    movwf   DELAY	;create scroll effect
    call    wait
    goto    splash
    
start_cont:
    bsf     bfcmode ;editor mode
    clrf    INST
    clrf    EEADR
    movlw   0xFF
    movwf   DELAY ;delay ~0.5 second for splash screen
    call    wait
    movlw   0xFF
    movwf   DELAY
    call    wait
    call    resetLCD ;clear lcd

disp:
    bcf	    STATUS, C
    rrf     INST, W
    movwf   EEADR
    bsf     STATUS, RP0 ;bank 1
    bsf     EECON1, RD  ;read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0
    btfss   INST, 0 ;check if even and swap
    swapf   EEDATA, F
    movfw   EEDATA ;check for EOF
    andlw   0x0F
    sublw   0x08
    btfsc   STATUS, Z
    goto    edit_start
    movfw   EEDATA
    andlw   0x07
    movwf   BUFF
    call    lcd_print_cmd
    incf    INST, F
    goto    disp
    
edit_start:
    movlw   0xFF
    movwf   DELAY ;delay ~0.5 second
    call    wait
    movlw   0xFF
    movwf   DELAY
    call    wait
    call    resetLCD ;clear lcd
    clrf    INST
    clrf    EEADR   ;EEPROM
    bsf     INTCON, INTE    ;enable ext. interrupt on RB0
    bsf	    INTCON, RBIE    ;enable port B change interrupt
    bsf     INTCON, GIE	    ;global interrupt enable

idle:
    btfss   bfcmode ;check mode
    goto    run
    goto    idle

run:
    bcf	    loopskip	;clear loop skip flag
    bcf	    statusled
    clrf    INST ;clear instruction pointer
    movfw   STA	;clear stack
    andlw   0xF0
    movwf   STA
    movlw   cellstart
    movwf   FSR	    ;clear RAM
    clrf    INDF
    incf    FSR
    movfw   FSR
    sublw   cellend+1
    btfss   STATUS, Z
    goto    $-5
    movlw   cellstart
    movwf   FSR	;move cell pointer to start
    clrf    EEADR
   
run_loop:
    ;Begin running BF code
    bcf	    STATUS, C
    rrf     INST, W
    movwf   EEADR
    bsf     STATUS, RP0 ;bank 1
    bsf     EECON1, RD  ;read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0	;bank 0
    movfw   EEDATA
    btfss   INST, 0 ;If even instruction, swap nibbles
    swapf   EEDATA, W
    andlw   0x0F
    btfsc   loopskip
    goto    run_loop_skip
    call    bf_decode
    incf    INST, F
    btfsc   bfcmode ;check mode
    goto    idle	;stop running
    goto    run_loop ;continue
    
run_loop_skip:
    sublw   0x05 ;check for ]
    btfsc   STATUS, Z
    bcf	    loopskip ;break out of skip loop
    incf    INST, F
    goto    run_loop	;continue

isr_editor:     ;write command (in W) to eeprom and LCD (as ASCII)
    movwf   BUFF  ;store input temporarily
    bcf	    STATUS, C
    rrf     INST, W
    movwf   EEADR
    bsf     STATUS, RP0 ;bank 1
    bsf     EECON1, RD  ;read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0

    btfsc   INST, 0 ;continue if even instruction, else write_odd
    goto    write_odd
    swapf   BUFF, W
    xorwf   EEDATA, W
    andlw   0xF0    ;keep upper nibble
    xorwf   EEDATA, F
    goto    isr_editor_cont1

write_odd:
    movfw   BUFF
    xorwf   EEDATA, W
    andlw   0x0F    ;keep lower nibble
    xorwf   EEDATA, F

isr_editor_cont1:
    bsf     STATUS, RP0 ;bank 1
    bsf     EECON1, WREN ;enable eeprom write
    bcf	    INTCON, GIE	;disable interrupts
    movlw   0x55
    movwf   EECON2
    movlw   0xAA
    movwf   EECON2 ;EEPROM write initialization procedure
    bsf     EECON1, WR	;do the write
    bsf	    INTCON, GIE	;enable interrupts
    bsf     EECON1, RD  ;back to read mode
    bcf     STATUS, RP0
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
    call    resetLCD   ;clear the screen
    retfie

isr_input:      ;run-mode input ISR
    retfie
    
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

resetLCD:    ;initialize LCD - E, RS and D4-D7 to lower 6 bits of 4094
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
    return
    
bf_end:	    ;reached end of program, return to idle
    bsf	    bfcmode
    bsf	    statusled	;indicate program done
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

addinst:    ;write new instruction
    return
    
    END
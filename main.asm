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

    LIST    P=PIC16F84, R=hex
    INCLUDE "p16F84.inc"   ;include chip-specific constants
    
    __CONFIG _CP_OFF & _PWRTE_ON & _WDT_OFF & _HS_OSC
    ;Fuses: code protect off, poweron timer on, watchdog off, high-speed xtal

#define srdata PORTB, 2 ;Actual pin 8
#define srclock PORTB, 3 ;Actual pin 9
#define srlatch PORTB, 1 ;Actual pin 7

#define lcdmode CELL, 7 ;LCD status bit (command/data)
#define bfcmode CELL, 6 ;mode bit (edit/run)

; Various special registers:
COUNT   EQU 0x0C    ;counter value
DELAY   EQU 0x0D    ;millisecond delay
BUFF	EQU 0x0E    ;EEPROM/LCD data buffer
STA     EQU 0x0F    ;Stack pointer (lower 3 bits)
CELL    EQU 0x10    ;brainfuck cell pointer - upper 2 status bits
INST    EQU 0x11    ;brainfuck "program counter" and input pointer

    ORG 0x2100 ;preload in eeprom
    de 0x44

    ORG 0x00
    goto	start

    ORG 0x04 ;ISR
    
    btfss   INTCON, INTF ;external interrupt (buttons) or onchange?
    goto    isr2    ;skip
    bcf     INTCON, INTF ;clear ext interrupt flag
    movfw   PORTA   ;quickly, read input into W
    andlw   0x07    ;keep 3 bits
    btfsc   bfcmode     ;check mode
    goto    isr_editor  ;edit
    goto    isr_input   ;input
isr2:    ;continue
    movlw   0x3C
    movwf   BUFF
    call    lcd_write
    bcf     INTCON, INTF
    
    retfie ;return

msg:					;splash msg
    addwf   PCL, f
    dt "PIC Brainfuck", 0

start:
    bsf     STATUS, RP0	;Bank 1
    movlw   0x01	; input on pin 0
    movwf   TRISB	;set PORTB output
    movlw   0x0F
    movwf   TRISA	;set PORTA input
    bcf     STATUS, RP0	;bank 0
    clrf    PORTB
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
    movfw   EEDATA
    btfss   INST, 0
    swapf   EEDATA, W
    andlw   0x07
    movwf   BUFF

    ;Write BUFF to LCD:
    movfw   BUFF
    btfsc   BUFF, 2 ;check 3rd bit for instruction type
    goto    xptrops
    addlw   0x03 ;convert to ascii
    iorlw   0x28
    movwf   BUFF
    goto    xisr_editor_cont ;skip ptrops

xptrops:     ;handles pointer control commands (< > and [ ])
    iorlw   0x3C ;< >
    btfsc   BUFF, 0 ;check 0th bit
    xorlw   0x60 ;_ ]
    movwf   BUFF
    sublw   0x5F ;correct _ into [
    btfsc   STATUS, Z
    bcf     BUFF, 2

xisr_editor_cont:
    call    lcd_write
    incf    INST, F
    movfw   INST
    sublw   0x0F
    btfss   STATUS, Z
    goto    disp
    
    movlw   0xFF
    movwf   DELAY ;delay ~0.5 second for splash screen
    call    wait
    movlw   0xFF
    movwf   DELAY
    call    wait
    call    resetLCD ;clear lcd
    clrf    INST
    clrf    EEADR   ;EEPROM
    bsf     INTCON, INTE    ;enable ext. interrupt on RB0
    bsf     INTCON, GIE	    ;global interrupt enable

idle:
    nop
    goto idle

run_loop:
    bcf     bfcmode ;run mode
    clrf    INST ;clear instruction pointer
    clrf    STA
    clrf    EEADR

    goto    idle

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
    andlw   0xF0    ;keep lower nibble
    xorwf   EEDATA, F
    goto    isr_editor_cont1

write_odd:
    movfw   BUFF
    xorwf   EEDATA, W
    andlw   0x0F    ;keep upper nibble
    xorwf   EEDATA, F

isr_editor_cont1:
    bsf     STATUS, RP0 ;bank 1
    bsf     EECON1, WREN ;enable eeprom write
    movlw   0x55
    movwf   EECON2
    movlw   0xAA
    movwf   EECON2 ;initialization procedure
    bsf     EECON1, WR
    bcf     STATUS, RP0

    bsf     STATUS, RP0 ;bank 1
    bsf     EECON1, RD  ;read EEPROM, EEDATA contains byte
    bcf     STATUS, RP0
    movfw   EEDATA
    btfss   INST, 0
    swapf   EEDATA, W
    andlw   0x07
    movwf   BUFF
    
    
    ;Write BUFF to LCD:
    movfw   BUFF
    btfsc   BUFF, 2 ;check 3rd bit
    goto    ptrops
    addlw   0x03 ;convert to ascii
    iorlw   0x28
    movwf   BUFF
    goto    isr_editor_cont2 ;skip ptrops

ptrops:     ;pointer control commands (< > and [ ])
    iorlw   0x3C ;< >
    btfsc   BUFF, 0 ;check 0th bit
    xorlw   0x60 ;_ ]
    movwf   BUFF
    sublw   0x5F ;correct _ into [
    btfsc   STATUS, Z
    bcf     BUFF, 2

isr_editor_cont2:
    call    lcd_write
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

isr_input:      ;write byte to cell
    retfie

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
    bsf     lcdmode ;back to lcd data mode
    return

lcd_line2: ;move lcd cursor to the 2nd line
    bcf     lcdmode  ;lcd command
    movlw   0xA8
    movwf   BUFF
    call    lcd_write
    bsf     lcdmode ;back to data mode
    return

;inc_ptr ;increment data pointer (<)
;    return
;
;dec_ptr ;decrement data pointer (>)
;    return
;
;inc_cell ;increment byte (+)
;    return
;
;dec_cell ;decement byte (-)
;    return
;
;out_cell ;display byte on LCD  (.)
;    return
;
;in_cell ;load input into byte (,)
;    return

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
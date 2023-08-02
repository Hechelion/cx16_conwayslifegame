; Title: Conway game life screensaver
; Language: Assembly
; System: Commander X16
; Version: V1.0.1 Emulator R43
; Author: Hechelion (hechelion@gmail.com)
; Date: 2023-08-01
; Compiler: CC65
; Build using:	cl65 -t cx16 -o CONWAYGL.PRG -l conwaygl.list conwaygl.asm


.org $080D
.segment "STARTUP"
.segment "INIT"
.segment "ONCE"
.segment "CODE"

; VERA Registers
VERA_LO         = $9F20
VERA_MID        = $9F21
VERA_HI         = $9F22
VERA_DATA_0     = $9F23
VERA_CTRL       = $9F25
VERA_DCVIDEO    = $9F29

VERA_L0_CFG     = $9F2D

; Kernal Functions
JOYSTICK_GET    = $FF56
ENTROPY_GET     = $FECF
KBDBUF_PEEK     = $FEBD


; Memory Locations
IRQ_VECTOR      = $0314
IRQ_HANDLER     = $30

P0_LO           = $22
P0_HI           = $23
P1_LO           = $24
P1_HI           = $25
TBL_DATA        = $8000
TBL_DATATEMP    = $8600


; VRAM Locations
;SPRITE_GRAPHICS = $4000
;SPRITE1         = $FC08

.macro P0_TO_DATA
    ;Load en page 0 $22 init position for grid data
    lda #<TBL_DATA
    sta P0_LO
    lda #>TBL_DATA
    sta P0_HI
.endmacro

.macro P1_TO_DATATEMP
    ;Load en page 0 $24 init position for temporal grid data
    lda #<TBL_DATATEMP
    sta P1_LO
    lda #>TBL_DATATEMP
    sta P1_HI
.endmacro

.macro INC_P0
    ;Increase *P0 (16 bits)
    inc P0_LO
    bne :+
    inc P0_HI
:   nop
.endmacro

.macro INC_P1
    ;Increase *P1 (16 bits)
    inc P1_LO
    bne :+
    inc P1_HI
:   nop
.endmacro

jmp main

Temp:       .byte $00
Tx:         .byte $00
T0:         .byte $00
Vel:        .byte $0A
Pausa:      .byte $00
Nc_lo:      .byte $00
Demo:       .byte $01

Total_lo:   .byte $00
Total_hi:   .byte $00
Aux_lo:     .byte $00
Aux_hi:     .byte $00
Equaltime:  .byte $00

;******************************************************************************
; MAIN
;******************************************************************************
main:
    ; INIT CONFIG
    lda #$40                            ; 2X Scale Horizontally and Vertically
    sta $9F2A
    sta $9F2B

    lda #%00010000                      ;Layer 0 to 64*32 tiles, 16 color, no bitmap, 1 bpp
    sta VERA_L0_CFG

    lda VERA_DCVIDEO
    ora #%00010000                      ; Layer 0 Enable
    and #%11011111                      ; Layer 1 Disable
    sta VERA_DCVIDEO

    lda #$20                            ; Layer 0, Map base at $4000
    sta $9F2E

    lda $9F36                           ; Tile map from layer 1
    sta $9F2F                           ; Tile map to layer 0

;*******************************************************************************
; Change pixel for tile
;*******************************************************************************
    stz VERA_CTRL                       ; Use Data Register 0
    lda #$11
    sta VERA_HI                         ; Set Increment to 1
    lda #$F0
    sta VERA_MID                        ; Set High Byte to $F8
    stz VERA_LO                         ; Set Low Byte to $00

    ldx #0
:   lda Live,x                          ; read from Live Data
    sta VERA_DATA_0                     ; Write to VRAM with +1 Autoincrement
    inx
    cpx #16
    bne :-

;*******************************************************************************
; Set color for tilemap
;*******************************************************************************
    stz VERA_CTRL                       ; Use Data Register 0
    lda #$10
    sta VERA_HI                         ; Set Increment to 1
    lda #$40
    sta VERA_MID                        ; Set VRAM $04000
    stz VERA_LO                         ;

    ldy #0
col:
    ldx #0
:   lda #0
    sta VERA_DATA_0
    lda #$BD
    sta VERA_DATA_0
    inx
    cpx #64
    bne :-

    iny
    cpy #32
    bne col

    jsr rand_fill                       ;Fill grid table with random values
    jsr game_to_vera                    ;load VERA with grid table values

    ; insert custom IRQ handler
    sei
    lda IRQ_VECTOR
    sta IRQ_HANDLER
    lda #<mainloop
    sta IRQ_VECTOR
    lda IRQ_VECTOR+1
    sta IRQ_HANDLER+1
    lda #>mainloop
    sta IRQ_VECTOR+1
    cli

    sei
    lda #<keyhandler
    sta $032e
    lda #>keyhandler
    sta $032f
    cli
    rts


:   nop                                 ;infinit main loop
    jmp :-

    rts

;*******************************************************************************
; SUB RUTINAS
;*******************************************************************************
;Fill grid table with random values
rand_fill:
    stz Tx
    P0_TO_DATA

cbyte:
    jsr ENTROPY_GET                     ;get entropy
    stx Temp
    eor Temp
    sta Temp

    and #%00000001                      ;bit 0
    sta (P0_LO)
    INC_P0
    ldy #1
cbits:
    lda Temp                            ;bit 1 a bit 7
    lsr
    sta Temp
    and #%00000001
    sta (P0_LO)
    INC_P0
    iny
    cpy #8
    bne cbits

    ldx Tx
    inx
    stx Tx
    cpx #$A0
    ;cpx #$A0                           ;5bytes(40 bits)*32 col = 160 o 0xA0
    bne cbyte

    rts

;*******************************************************************************
;Load VERA tilemap memory with grid table values
game_to_vera:
    lda #$20
    sta VERA_HI
    lda #$40
    sta VERA_MID
    lda #$00
    sta VERA_LO

    P0_TO_DATA                          ;*P0 reset to grid table

    ldy #0
fila:
     ldx #0
colm:
    lda (P0_LO)
    sta VERA_DATA_0
    INC_P0

    inx
    cpx #40
    bne colm                            ;40 tiles with

:   stz VERA_DATA_0
    inx
    cpx #64                             ;from position 40 to 64 load #0 in VERA tilemap memory
    bne :-
    ;Fin de una fila

    iny
    cpy #30                             ;30 tiles hight
    bne fila

    rts

;*******************************************************************************
;Load grid table with temp table
mov_data:
    P0_TO_DATA                          ;*P0 reset to grid table
    P1_TO_DATATEMP                      ;*P1 reset to temporal table

cn:
    lda (P1_LO)
    sta (P0_LO)
    INC_P0
    INC_P1

    lda P0_HI
    cmp #$85
    bne cn

    rts

;*******************************************************************************
; 1 step conway rules
update:
    ;reset count of total live cell
    stz Aux_lo
    stz Aux_hi
    ;copy first 2 rows to end of grid table
    ldx #0
:   lda $8000,x
    sta $84B0,x
    inx
    cpx #81
    bne :-

    P0_TO_DATA                          ;*P0 reset to grid table
    lda #$29                            ;*P1 reset to temporal table + 41 bytes
    sta P1_LO
    lda #$86
    sta P1_HI

    stz Tx

nextfile:
    ldx #0
next:
    ;***** Conway rules
    stz Temp                            ;Reset total neighbors lives

    ;count live neighbors
    ldy #0                              ;Up-Left cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #1                              ;Up cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #2                              ;Up-Right cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #40                             ;Left cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #42                             ;Right cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #80                             ;Down-Left cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #81                             ;Down cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ldy #82                             ;Down right cell
    lda (P0_LO),y
    adc Temp
    sta Temp

    ;Dead or live rule
    cmp #2
    beq nchange
    cmp #3
    beq live
;dead:
    lda #0                              ;dead cell
    sta (P1_LO)
    jmp edl
nchange:
    ldy #41
    lda (P0_LO),y
    sta (P1_LO)                         ;Keep cell value
    jmp edl
live:
    lda #1                              ;live cell
    sta (P1_LO)
edl:
    adc Aux_lo                          ;contador de total de celdas vivas
    sta Aux_lo
    bcc :+
    inc Aux_hi

:   INC_P0                              ;incrementamos los punteros de memoria
    INC_P1

    inx
    cpx #40
    beq :+
    jmp next

:   lda Tx
    inc
    sta Tx
    cmp #30
    beq :+
    jmp  nextfile

    ;Copy #31 row to first row
:   ldx #0
:   lda $8A60,x
    sta $8600,x
    inx
    cpx #41
    bne :-

    rts

;*******************************************************************************
; Main game loop
mainloop:
    ;Step*Step
    lda #1
    cmp Pausa
    bne :+
    lda #0
    sta Pausa
    bra siup

:   lda #0                              ;Check PAUSE
    cmp Vel
    beq noup

    lda T0                              ;wait time
    inc
    sta T0
    cmp Vel
    bmi noup
siup:
    stz T0                              ;Reset wait time

    ;compare total live cell vs total live cell for previus cycle
    lda Aux_lo
    cmp Total_lo
    bne :+
    lda Aux_hi
    cmp Total_hi
    bne :+
    ;If the same value, inc. Equaltime
    inc Equaltime
    lda Equaltime
    cmp #10
    bne :+
    ;If same valur for 10 cycles, then reset game
    stz Equaltime
    stz Nc_lo
    jsr rand_fill

    ;Memorice the previus count of total lived cell
:   lda Aux_lo
    sta Total_lo
    lda Aux_hi
    sta Total_hi

    inc Nc_lo                           ;increase cycles count
    bne :+
    lda Demo                            ;if Demo mode enable, set memory with random number
    cmp #1
    bne :+
    jsr rand_fill

    ;Game subrutines
:   jsr update
    jsr mov_data
    jsr game_to_vera
noup:
    jmp (IRQ_HANDLER)

keyhandler:
    and #$ff                            ;ensure A sets flags
    bmi exit                            ;A & 0x80 is key up

    cmp #$02                            ;key_1 (60 FPS)
    bne :+
    lda #1
    sta Vel
    jmp exit

:   cmp #$03                            ;key_2 (12 FPS)
    bne :+
    lda #5
    sta Vel
    jmp exit

:   cmp #$04                            ;key_3 (6 FPS)
    bne :+
    lda #10
    sta Vel
    jmp exit

:   cmp #$05                            ;key_4 (2 FPS)
    bne :+
    lda #30
    sta Vel
    jmp exit

:   cmp #$06                            ;key_5 (Pausa and step*step loop)
    bne :+
    lda #0
    sta Vel
    lda #1
    sta Pausa
    jmp exit

:   cmp #$11                            ;key_Q, exit game
    bne :+
    LDX #$42                            ; System Management Controller
    LDY #$02                            ; magic location for system reset
    LDA #$00                            ; magic value for system poweroff
    JSR $FEC9                           ; power off the system
    jmp exit

:   cmp #$14                            ;Key_R Reset game
    bne :+
    stz Nc_lo
    jsr rand_fill
    jmp exit

:   cmp #$21                            ;Key_R Alternate demo mode
    bne exit
    lda Demo
    cmp #1
    bne :+
    stz Demo
    jmp exit
:   lda #1
    sta Demo
    jmp exit

exit:
    rts

;*******************************************************************************
; DATOS
;*******************************************************************************
Live:
    .byte $00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$18,$3C,$7E,$7E,$3C,$18,$00


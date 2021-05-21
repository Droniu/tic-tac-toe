.segment "HEADER" ; what code is
    .byte "NES" ; beginning of the NES Header
    .byte $1a ; signature of NES Header
    .byte $02 ; 2*16 KiB PRG ROM
    .byte $01 ; 1*8 KiB CHR ROM
    .byte %00000000 ; mapper and mirroring
    .byte $00 ; not important in this project
    .byte $00
    .byte $00
    .byte $00
    .byte $00, $00, $00, $00, $00 ; filler bytes
.segment "ZEROPAGE"
    background: .res 2
    pressing: .res 1
.segment "STARTUP" ; where code starts
    Reset:
        sei ; disables all interrupts on NES
        cld ; disable decimal mode - NES doesn't support it
        
        ldx #$40 ; disable sound IRQ (load -> reg. x, store -> 4017)
        stx $4017 

        ldx #$FF ; initialize stack register
        txs ; transfer x stack

        inx ; ff + 1 = 00

        ; Clean PPU registers
        stx $2000
        stx $2001

        stx $4010 ; Disable PCM channel
        
        : ; anonymous label
        bit $2002; $2002 is telling if PPU is currently drawing
        bpl :- ; branch if not in VBLANK (not waiting for another screen)
        ; :- means go to last anonymous label, 
        ; :+ go to next anonymous label

        txa 

    ; Memory clearing
    CLEARMEM:
        sta $0000, X ; $0000 -> $00FF
        sta $0100, X

        sta $0300, X
        sta $0400, X
        sta $0500, X 
        sta $0600, X
        sta $0700, X

        ; one of the ranges is for sprie data. we can choose any
        lda #$FF
        sta $0200, X
        lda #$00

        inx
        bne CLEARMEM 

    ; wait for VBLANK again
    :
        bit $2002
        bpl :-

        lda #$02 ; MSB of sprite data memory range
        sta $4014 ; Object Attribute Memory DMA register
        nop ; ppu needs a moment to load data

        ; preparing ppu memory 
        lda #$3F
        sta $2006
        lda #$00
        sta $2006

        ldx #$00

    LoadPalettes:
        lda PaletteData, X
        sta $2007 ; $3F00, $3F01, and so on

        ; loop
        inx
        cpx #$20 ; 32 in decimal
        bne LoadPalettes

        ; initialize world variable to point to world data
        lda #<BGData
        sta background
        lda #>BGData
        sta background+1

        ; setup address in PPU for nametable data
        bit $2002
        lda #$20
        sta $2006
        lda #$00
        sta $2006        

        ldx #$00
        ldy #$00

    LoadBackground:
        lda (background), Y
        sta $2007
        iny
        ; 960 px is 03 C0 in hex
        cpx #$03
        bne :+
        cpy #$C0
        beq DoneLoadingBG
    :
        cpy #$00
        bne LoadBackground
        inx
        inc background+1
        jmp LoadBackground


    DoneLoadingBG:
        ldx #$00
    
    LoadSprites:

        lda SpriteData, X
        sta $0200, X
        inx
        cpx #$20
        bne LoadSprites
    
    ; enable interrupts
        cli
        lda #%10010000 ; enable NMI, use second set of sprites
        sta $2000
        ; enable drawing in leftmost 8px of screen and in general
        lda #%00011110
        sta $2001
        

    Forever:
        jmp Forever ; prevents going to NMI after pushing reset

    ; ; ; ; ; ; ; ; ; ; ; ;
    ; SUBROUTINES SECTION ;
    ; ; ; ; ; ; ; ; ; ; ; ;
    
    CheckController:
        lda #$01
        sta $4016 ; strobe controller
        ldx #$00
        stx $4016 ; latch controller status
    ConLoop:
        ; reading 4016 eight times in order to check buttons.
        ; we use carry flag to write button status to variables
        ; first we use lsr to shift LSB to carry flag, then
        ; we use ROR to shift 0 to carry and carry to variable.
        
        lda $4016 ; %0000001 
        lsr
        ror pressing  ; RLDUSsBA
        inx
        cpx #$08
        bne ConLoop
        
        CheckRight:
            lda #%10000000
            and pressing
            beq CheckLeft
        CheckLeft:
            lda #%01000000
            and pressing
            beq CheckDown
        CheckDown:
            lda #%00100000
            and pressing
            beq CheckUp
        CheckUp:
            lda #%00010000
            and pressing
            beq CheckStart
        CheckStart:
            lda #%00001000
            and pressing
            beq CheckSelect
        CheckSelect:
            lda #%00000100
            and pressing
            beq CheckB
        CheckB:
            lda #%00000010
            and pressing
            beq CheckA
        CheckA:
            lda #%00000001
            and pressing
            


    ; ; ; ; ; ; ; ; ; ; ; ;
    ; INTERRUPTS SECTION  ;
    ; ; ; ; ; ; ; ; ; ; ; ;

    NMI:
        ; loading sprites from $0200 to ppu memory
        lda #$02 ; (2 because this is MSB)
        sta $4014

        jsr CheckController
        
        rti ; interrupt return


    ; ; ; ; ; ; ; ; ; ; ; ;
    ;   DATA / INCLUDES   ;
    ; ; ; ; ; ; ; ; ; ; ; ;

    PaletteData: ;example
        ; example background palette data
        .byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F
        ; example sprite palette data  
        .byte $22,$16,$27,$18,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17
    ; 1st byte: y-offset
    ; 2nd byte: tile
    ; 3rd byte: ?
    ; 4th byte: x-offset
    SpriteData: ; example
        .byte $BF, $04, $00, $80

    BGData:
        .incbin "grid.bin"


.segment "VECTORS" ; special address which 6502 needs
    .word NMI ; refresh time
    .word Reset ; reset button
.segment "CHARS" ; graphical data
    .incbin "tictactoe.chr"
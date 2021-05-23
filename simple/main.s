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
    gameLoaded: .res 1
    menuBg: .res 2
    gameBg: .res 2
    pressing: .res 1
    position: .res 1
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
        
        stx gameLoaded

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

    LoadMenu:
        ; initialize world variable to point to world data
        lda #<MenuData
        sta menuBg
        lda #>MenuData
        sta menuBg+1

        ; setup address in PPU for nametable data
        bit $2002
        lda #$20
        sta $2006
        lda #$00
        sta $2006        

        ldx #$00
        ldy #$00

    LoadMenuLoop:
        lda (menuBg), Y
        sta $2007
        iny
        ; 960 px is 03 C0 in hex
        cpx #$03
        bne :+
        cpy #$C0
        beq MenuDone
    :
        cpy #$00
        bne LoadMenuLoop
        inx
        inc menuBg+1
        jmp LoadMenuLoop


    MenuDone:
        ldx #$00

    LoadGame:
        ; initialize world variable to point to world data
        lda #<GridData
        sta gameBg
        lda #>GridData
        sta gameBg+1

        ; setup address in PPU for nametable data
        bit $2002
        lda #$28
        sta $2006
        lda #$00
        sta $2006        

        ldx #$00
        ldy #$00

    LoadGameLoop:
        lda (gameBg), Y
        sta $2007
        iny
        ; 960 px is 03 C0 in hex
        cpx #$03
        bne :+
        cpy #$C0
        beq FinishLoadingGame
    :
        cpy #$00
        bne LoadGameLoop
        inx
        inc gameBg+1
        jmp LoadGameLoop

    FinishLoadingGame:
    ; enable interrupts
        cli
        lda #%10000000 ; enable NMI
        sta $2000
        ; enable drawing in leftmost 8px of screen and in general
        lda #%00011110
        sta $2001
        


    Forever:
        jmp Forever ; game loop


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
           jsr MoveRight
        CheckLeft:
            lda #%01000000
            and pressing
            beq CheckDown
            jsr MoveLeft
        CheckDown:
            lda #%00100000
            and pressing
            beq CheckUp
            jsr MoveDown
        CheckUp:
            lda #%00010000
            and pressing
            beq CheckStart
            jsr MoveUp
        CheckStart:
            lda #%00001000
            and pressing
            beq CheckA
            jsr StartGame
        CheckA:
            lda #%00000001
            and pressing
            
    EndController:
        rts
    
    
    MoveRight:
        lda $0203
        adc #$0002
        sta $0203
        rts
    MoveLeft:
        lda $0203
        sbc #$0002
        sta $0203
        rts
    MoveUp:
        lda $0200
        sbc #$0002
        sta $0200
        rts
    MoveDown:
        lda $0200
        adc #$0002
        sta $0200
        rts
    StartGame:
        lda #$01
        cmp gameLoaded
        bne LoadGrid
        rts ; back if game already loaded
        LoadGrid:
            lda #%10000010 ; enable NMI
            sta $2000
            rts


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
        .byte $15,$30,$30,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F
        ; example sprite palette data  
        .byte $17,$16,$27,$30,$22,$30,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17
    ; 1st byte: y-offset
    ; 2nd byte: tile
    ; 3rd byte: attributes
    ; 4th byte: x-offset
    SpriteData: ; example
        .byte $BF, $15, $00, $80

    GridData:
        .incbin "grid.bin"
    MenuData:
        .incbin "main_menu.bin"


.segment "VECTORS" ; special address which 6502 needs
    .word NMI ; refresh time
    .word Reset ; reset button
.segment "CHARS" ; graphical data
    .incbin "tictactoe.chr"
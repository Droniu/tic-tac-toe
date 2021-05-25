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
    gameLoaded: .res 1 ; for disabling start after title screen
    menuBg: .res 2
    gameBg: .res 2
    pressing: .res 1
    pressingOld: .res 1
    position: .res 1 ; 0 - top left, 8 - bottm right
    turn: .res 1 ; 0 = x, 1 = o
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

        ; one of the ranges is for sprite data. we can choose any
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

        ldx #$00

    LoadSprites:
        lda SpriteData, X
        sta $0200, X
        inx
        cpx #$20
        bne LoadSprites

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
        lda #$04 ; default arrow position - middle tile
        ; will be changed on keyboard hits
        sta position
        lda #$00
        sta turn

        cli ; enable interrupts
        lda #%10000000 ; enable NMI
        sta $2000
        ; enable drawing (but no sprites -> 4th bit)
        lda #%00001110
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
        lda pressing
        sta pressingOld
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
            lda pressing
            eor pressingOld
            and pressing
            and #%10000000
            beq CheckLeft
            jsr MoveRight
        CheckLeft:
            lda pressing
            eor pressingOld
            and pressing
            and #%01000000
            beq CheckDown
            jsr MoveLeft
        CheckDown:
            lda pressing
            eor pressingOld
            and pressing
            and #%00100000
            beq CheckUp
            jsr MoveDown
        CheckUp:
            lda pressing
            eor pressingOld
            and pressing
            and #%00010000
            beq CheckStart
            jsr MoveUp
        CheckStart:
            lda pressing
            eor pressingOld
            and pressing
            and #%00001000
            beq CheckA
            jsr StartGame
        CheckA:
            lda pressing
            eor pressingOld
            and pressing
            and #%00000001
            beq EndController
            jsr PressA
            
    EndController:
        rts
    
    
    MoveRight:
        lda $0203
        cmp #$B4 ; 180 pixels, max right position
        bne :+
        sbc #$0070 ; go max left
        sta $0203
        rts
        :
        adc #$0038 ; 56 pixels (7 tiles) to the right
        sta $0203
        rts
    MoveLeft:
        lda $0203
        cmp #$44 ; 68 pixels, max left position
        bne :+
        lda #$B4
        sta $0203
        rts
        :
        sbc #$0038
        sta $0203
        rts
    MoveUp:
        lda $0200
        cmp #$32 ; 50 pixels, max up position
        bne :+
        lda #$A2
        sta $0200
        rts
        :
        sbc #$0038
        sta $0200
        rts
    MoveDown:
        lda $0200
        cmp #$A2 ; 162 pixels, max down position
        bne :+
        lda #$32
        sta $0200
        rts
        :
        adc #$0038
        sta $0200
        rts
    StartGame:
        lda #$01
        cmp gameLoaded
        bne LoadGrid
        rts ; back if game already loaded
        LoadGrid:
            sta gameLoaded
            bit $2002
            lda #$00
            sta $2005
            lda #$EF ; scroll to 2nd screen
            sta $2005
            lda #%10000000 ; default values
            sta $2000
            lda #%00011110 ; enable sprites
            sta $2001
            rts

    PressA:
        lda gameLoaded
        cmp #$00
        bne :+
        rts
    :   lda turn
        ;cmp #$00
        ;bne :+ 
        jsr DrawX
        jsr LoadGrid ; load defaults
        rts


    DrawX:
        lda position
        cmp #$04
        beq DrawX4
        DrawX4:
            ldx #$00
            bit $2002
        :   lda #$29
            cpx #$08 
            bmi :+
            adc #$00 ; cpx sets carry to 1 so its +1 actually
        :   sta $2006
            lda center, X
            sta $2006
            lda xspr, X
            sta $2007
            inx
            cpx #$10
            bne :--
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
        ; background palette data
        .byte $15,$30,$30,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F
        ; sprite palette data  
        .byte $17,$16,$27,$30,$22,$30,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17

    ; 1st byte: y-offset
    ; 2nd byte: tile
    ; 3rd byte: attributes
    ; 4th byte: x-offset

    topleft:
        .byte $e7,$e8,$e9,$ea
        .byte $07,$08,$0a,$0b
        .byte $27,$28,$29,$2a
        .byte $47,$48,$49,$4a
    top:
        .byte $ee,$ef,$f0,$f1
        .byte $0e,$0f,$10,$11
        .byte $2e,$2f,$30,$31
        .byte $4e,$4f,$50,$51
    topright:
        .byte $f5,$f6,$f7,$f8
        .byte $15,$16,$17,$18
        .byte $35,$36,$37,$38
        .byte $55,$56,$57,$58
    left:
        .byte $c7,$c8,$c9,$ca
        .byte $e7,$e8,$e9,$ea
        .byte $07,$08,$09,$0a
        .byte $27,$28,$29,$2a
    center:
        .byte $ce,$cf,$d0,$d1
        .byte $ee,$ef,$f0,$f1
        .byte $0e,$0f,$10,$11
        .byte $2e,$2f,$30,$31
    right:
        .byte $d5,$d6,$d7,$d8
        .byte $f5,$f6,$f7,$f8
        .byte $15,$16,$17,$18
        .byte $35,$36,$37,$38
    bottomleft:
        .byte $a7,$a8,$a9,$aa
        .byte $c7,$c8,$c9,$ca
        .byte $e7,$e8,$e9,$ea
        .byte $07,$08,$08,$0a
    bottom:
        .byte $ae,$af,$b0,$b1
        .byte $ce,$cf,$d0,$d1
        .byte $ee,$ef,$f0,$f1
        .byte $0e,$0f,$10,$11
    bottomright:
        .byte $b5,$b6,$b7,$b8
        .byte $d5,$d6,$d7,$d8
        .byte $f5,$f6,$f7,$f8
        .byte $15,$16,$17,$18
    clear:
        .byte $00,$00,$00,$00
        .byte $00,$00,$00,$00
        .byte $00,$00,$00,$00
        .byte $00,$00,$00,$00

    xspr:
        .byte $80,$81,$82,$83
        .byte $90,$91,$92,$93
        .byte $a0,$a1,$a2,$a3
        .byte $b0,$b1,$b2,$b3

    ospr:
        .byte $84,$85,$86,$87
        .byte $94,$95,$96,$97
        .byte $a4,$a5,$a6,$a7
        .byte $b4,$b5,$b6,$b7
    
    SpriteData:
        .byte $6A, $71, $00, $7C    ; selection arrow
                                    ; $0200 - y, $0203 - x
        ;.byte 
        


    GridData:
        .incbin "grid.bin"
    MenuData:
        .incbin "main_menu.bin"


.segment "VECTORS" ; special address which 6502 needs
    .word NMI ; refresh time
    .word Reset ; reset button
.segment "CHARS" ; graphical data
    .incbin "tictactoe.chr"
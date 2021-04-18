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
        stx #$2000
        stx #$2001
    
    Forever:
        jmp Forever ; prevents going to NMI after pushing reset

    NMI:
        rti ; interrupt return


  
.segment "VECTORS" ; special address which 6502 needs
    .word NMI ; refresh time
    .word Reset ; reset button
.segment "CHARS" ; graphical data
    .incbin "graphics.chr"
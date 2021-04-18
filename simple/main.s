.segment "HEADER"
  .byte "NES" ; 
  .byte $1a ; 
.segment "ZEROPAGE"
.segment "STARTUP"
.segment "VECTORS"
.segment "CHARS"
  .incbin "main.chr"
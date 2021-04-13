ca65 ttxo.asm
ld65 -C ttxo.cfg -o ttxo.prg ttxo.o
copy /b ttxo.hdr+ttxo.prg+g\ttxo.chr+g\ttxo.chr "Tic-Tac XO.nes"
pause

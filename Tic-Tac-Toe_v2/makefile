all: main

main: main.asm
	ca65 main.asm -g -o main.o
	ld65 -C memory.cfg -o main.prg main.o
	cat ttxo.hdr main.prg graphics.chr > Tic-Tac-Toe.nes
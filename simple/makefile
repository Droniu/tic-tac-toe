all: main

main: main.s
	ca65 main.s -g -o main.o --debug-info
	ld65 main.o -o main.nes -t nes --dbgfile main.dbg
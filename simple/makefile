all: main

main: main.s
	ca65 main.s -g -o main.o -t nes
	ld65 main.o -o main.nes -t nes
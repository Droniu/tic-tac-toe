all: ttxo

ttxo: ttxo.asm
	ca65 ttxo.asm -g -o ttxo.o
	ld65 -C ttxo.cfg -o ttxo.prg ttxo.o
	cat ttxo.hdr ttxo.prg ttxo.chr > ttxo.nes




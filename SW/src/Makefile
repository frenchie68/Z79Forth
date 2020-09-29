SHELL	= /bin/sh

all:	forth.hex

rebuild: clean all

clean:
	rm -f forth.hex

forth.hex: forth.asm console.asm storage.asm constants.asm
	lwasm -s --symbols-nolocals -l -f ihex -o $@ $< 2>&1 | \
	  tee `basename $@ .hex`.lst


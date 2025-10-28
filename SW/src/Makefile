SHELL	= /bin/sh

all:	forth.hex

rebuild: clean all

clean:
	rm -f forth.hex forth.pro

forth.hex: forth.asm console.asm storage.asm rtc.asm constants.asm
	lwasm -s --symbols-nolocals -l -f ihex -o $@ $< 2>&1 | \
	  tee `basename $@ .hex`.lst

###############################################################################
# The 'minipro' programming software for the TL866II+ can be found at
# https://gitlab.com/DavidGriffith/minipro.git
# The following implements specific support for it.
# Special thanks go to David J. Richards for having opened my eyes to it!

# Discard all references to memory locations in $0XXX.
# In Z79Forth they are produced by lwasm when 'ALIGN' directives
# are encountered and this is only used in RAM space.

# For all records with a 'data' type (0), substract $8000 from the 'address'
# field provided it is in the range ['C'..''F']--physical to ROM (logical)
# address translation. Also convert the file from DOS text format to Unix.
forth.pro: forth.hex
	@sed -e 's/$$//; /^:\(..\)0\(...\)00/d; s/^:\(..\)C\(...\)00/:\14\200/; s/^:\(..\)D\(...\)00/:\15\200/; s/^:\(..\)E\(...\)00/:\16\200/; s/^:\(..\)F\(...\)00/:\17\200/' $< > $@; \
	./chksum-fix.py $@ > _$@; mv _$@ $@

prog: forth.pro
	minipro -P -u -z -p AT28C256 -s -f ihex -w forth.pro


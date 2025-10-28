SHELL	= /bin/sh

all:	forth.hex forth.pro

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
#
# For all records with a 'data' type (0), substract $E000 from the 'address'
# field, provided it is in the range ['E'..''F']--physical to ROM (logical)
# address translation. Also convert the file from DOS text format to Unix.
forth.pro: forth.hex
	@sed -e 's/$$//; /^:\(..\)0\(...\)00/d; s/^:\(..\)E\(...\)00/:\10\200/; s/^:\(..\)F\(...\)00/:\11\200/' $< > $@; \
	./chksum-fix.py $@ > _$@; mv _$@ $@

prog: forth.pro
	minipro -P -u -z -p AT28C64B -s -f ihex -w forth.pro


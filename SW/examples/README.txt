                    A Brief Introduction to the Example Programs

Some of these have been "blockified" and are present in the CompactFlash image
that can be found in SW/util/*.img, in which case credit to the original author
can be found at the beginning of each block and also in SW/util/cfinit.sh.

Example		Author		Notes
100fact.4th	Michel Jean	Computes 100!. This came up as an SVFIG
				challenge. The file has to be sourced from
				the console.

blkins.4th	Francois Laagel	Inserts one blank clock in front of a specified
				block range. Absolutely not guaranteed to do
				the right thing.

cftest.4th	Francois Laagel	Original prototype implementation for low level
				CompactFlash support. Kept for historical
				reference. Do not use this, as it is now
				implemented in the default EEPROM image!

coop-mtask.4th	Matthias Koch	A sample cooperative multitasker. A sample
				demo can be loaded from CF storage by resorting
				to COOPMTLOAD

dis.4th		Francois Laagel	The standard disassembler. It is normally loaded
				automatically from CompactFlash when the
				system comes up. Standard invokation is via:
				DIS <word>

dump.4th	Francois Laagel	An hexadecimal DUMP utility. It can be loaded
				from CF by resorting to DUMPLOAD
				Usage is: <addr> <bytecount> DUMP

enigma-f.4th	Bill Ragsdale	The current SVFIG's boss's take on the Enigma
				encryption system. The code has to be sourced
				from the console.

expsys.4th	Demitri Peynado	An expert system that tries to diagnose various
				forms of COVID-19/common flu. This is very
				interesting code in itself. It can be loaded
				from CF storage by resorting to: EXPSYSLOAD
				Side note: the added code supports the
				functionality of Forthwin's WORDBYADDR

fsformat.4th	F+L		A Forth source code formatter. This comes from
				the archives of "Forth Dimension" that can be
				reached at http://forth.org/fd/FDcover.html
				The code has to be sourced from the console.

hanoi.4th	Peter Midnight	The classic towers of Hanoi recursion demo.
				This also comes from the archives of "Forth
				Dimension", however it also benefits from a
				bug fix found in the VfxForth distribution.
				It can be loaded from CF storage by resorting
				to HANLOAD The application is started by
				issuing <nn> TOWERS where nn is a number
				between 2 and 10

life.4th	Paul E. Bennett An implementation of John Horton Comway's
				"Game of Life" and my first brush with ANSI
				Forth source code. The application can be
				started from CF storage by resorting to
				LIFELOAD It can be started over and over again
				by calling LIFE

lwvi.4th	Francois Laagel A lightweight implementation of the Berkeley
				VI editor, especially tuned for block editing.
				This utility is not entirely finished and is
				partially buggy! It has its own documentation
				in SW/doc/LWVI-UserManual.txt. It can be loaded
				from CF storage by resorting to VILOAD
				After that any block can be edited via:
				<nn> EDIT

mandel.4th	Martin H.	An ASCII based Mandelbrot fractal generation
				program (source is https://github.com/Martin-H1)
				The code has to be sourced from the console.

meta2		Demitri Peynado	A syntax directed compiler generator for
				ValgolI. This is a remarkable program that
				takes a syntax specification from blocks
				600-602 and uses Forth as an intermediary
				target language. Block 603 contains a sample
				program expressed in ValgolI. Further
				documentation can be found in SW/examples/meta2/
				README.txt The program has to be sourced from
				the console. It makes extensive use of the
				following ANSI primitives: DEFER IS :NONAME

palflt.4th	Francois Laagel	A palindromic number generator that operates
				on double cells. It is a bit CPU intensive
				since it a brute force approach. It also 
				uses <# #S #> to produce a string representation
				of every number under consideration. The
				application can be launched from CF storage
				by resorting to PALFLTLOAD

phrpal.4th	Pablo Hugo Reda	A palindromic number generator that operates
				one single cells. This is a much smarter
				approach that does not require number to
				string convertion. That application can be
				launched from CF storage by resorting to
				PHRPALLOAD

rc4.4th		Wikipedia's	An implementation of the RC4 encryption
		Forth page	algorithm. This needs to be sourced from
				the console. Interesting is the use of ANSI
				VALUE TO primitives.

rtc.4th		Francois Laagel	Experimental application level support for the
				Motorola MC146818 real-time clock. This relies
				on hardware support that I do not endorse.
				There are better solutions to this then what
				I had initially envisioned.

sapin.4th	Michel Jean	Michel's obfuscated Forth code Christmas gift
				from a few years ago. The application can be
				launched from CF storage by resorting to
				SAPINLOAD

test.4th	Francois Laagel Vestigial code used for validation purposes
				at some point. This should probably go...

vt100.4th	Francois Laagel	Vestigial code of little interest besides
				folks interested in terminal based editor
				development.

The CF image shipped in SW/util/*.img also includes a subset of the Forth2012
test suite. It is not meant as an ANSI compliance test but as a non-regression
test suite. It can be loaded from CF storage by resorting to: F12TESTLOAD


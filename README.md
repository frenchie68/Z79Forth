# Z79Forth

![An Early Hardware Iteration](KIMG0091.jpg "The Wire Wrapped Prototype Board")

## Overview ##
This project has a hardware component and a software component.

- __hardware__: an Hitachi 63C09 based single board computer, aka the *Z79Forth
  Reference Board.*
- __software__: an ANS94 full __Core__ word set implementation entirely written
  in assembly language. Some primitives of the __Core ext__ word set are also
  supplied.

Additionally, some sample application programs and benchmarks are provided.
Please note that the GPL version 3 only applies to the assembly code. The
schematics and the Forth source code are released in the public domain.

*The 6309 Book* has been included in this distribution with the express assent
of its author, Chris Burke--may he be hereby thanked one more time for a great
piece of work, without which none of this would have been possible!

Known shortcomings of the current repository are as follows:

- unfinished editor (see `SW/examples/lwvi.4th`). Some extra work still is
  needed on the delete primitives.
  
Please note that it is not recommended to clone master directly, as the
software may not match the schematics. The recommended approach is to clone
a release.

## Background
It all started by the end of 2019 as an effort aiming at resurrecting my
digital electronics development skills. Z79Forth is a tribute to the golden
days of eight bit computing. The Motorola MC6809 processor always had an
excellent reputation for being Forth friendly, being endowed with two stack
pointers and two index registers. I always wanted to program a serious
application for it but never had the chance. Hardware and software development
proceeded in parallel.

### Hardware Development
Z79Forth is a journey into retro-computing that takes advantage of modern
technology where appropriate (CMOS, USB and CompactFlash). Wire wrapping is
particularly adapted to digital hardware experimentation and provides the
developer with a great deal of flexibility.

The original MC6809 processor is hard to procure these days. It came under
two very distints variants: one requiring an external crystal (on chip
oscillator) and the __E__ suffixed version, meant to be used with an external
oscillator. For integration's sake I selected the __E__ flavor.

In addition to being tricky to buy, the MC6809 was also limited to a 2 MHz
bus clock. In contrast, Hitachi's own implementation of the 6809 architecture
supports bus clock frequencies of up to 3 MHz officially and 5 MHz in practice.
The Z79Forth reference board uses the HD63C09E clocked at 4 MHz. The choice of
that particular frequency was meant to allow some bus cycles to be stretched
down easily to 1 MHz, so as to be able to possibly accomodate extensions using
components of the MC6800 product line. At this point, it is worth mentioning
the fact that the HD6309 offers reduced power consumption and an extended
instruction set which is heavily used by the Z79Forth software.

The 6309 is a CMOS device but I wanted to be able to support TTL inputs. So,
most of the logic circuits are from the 74HCT family. The only notable
exception is with the Schmitt inverting triggers implemented in 74HC
technology.

I followed an incremental modular sub-system centric approach when designing
the platform.

- __power__: was initially provided via an external laboratory power supply.
  Later on, I decided to supply it over a USB FTDI module that also supports
  asynchronous serial communication with the host. A simple phone charger can
  also be used when the selected console is over RS-232.
- __clock__: there are two clock domains on the board: one for serial
  communication and one for the processor itself. Each clock domain is
  generated by the output of a dedicated crystal controlled oscillator (XCO).
  The asynchronous communication interface adapter (ACIA) transmit and receive
  clock inputs are driven by a 1.8432 MHz XCO, optionally divided by a factor
  of three. This provides support for either 115200 or 38400 bps communication.
  The CPU clock inputs are to be supplied in quadrature at 4 MHz. This is
  accomplished by dividing the output of a 16 MHz XCO via a dual JK flip-flop.
  Care has been taken to provide potential support for streching down the
  bus clock down to 1 MHz for 6800 peripherals.
- __address bus decoding__: a very early and rather central design decision
  was to select a workable memory map. The Z80 software required about 5 KB
  of memory and was RAM resident. I figured an 8 KB EEPROM ought to be enough
  to accomodate the 6809 implementation of Z79Forth, which led me to divide
  the addres space of the processor into eight 8 KB regions. Interrupt vectors
  have to be readable from a 16 byte segment starting at $FFF0, so the last
  region ($E000-$FFFF) needed to be assigned to the EEPROM. Region 6
  ($C000-$DFFF) is where memory mapped I/O operations take place. Regions 0-3
  ($0000-$7FFF) is populated by a 32 KB static RAM. Region 4 ($8000-$BFFF)
  remains available for off board extensions.

  The I/O space is further divided into eight 1 KB areas, allowing for up to
  eight devices to be used in the system. Of those, dev. 0 is reserved for
  the CompactFlash module; dev. 6 is assigned to serial communication. An
  unsupported RTC experiment based on the MC146818 chip uses dev. 5.
- __EEPROM__: a modern production grade Microchip AT28C64B 8 KB was selected
  (150 ns access time).
- __ACIA__: an HD63B50 IC was selected for its availibility and its improved
  flexibility over the MC6850. In particular its E input pin can be driven
  by a strobe signal and does not need to be a clocked signal. Recent (2.2.x)
  versions of the schematics include a jumper based multiplexer that
  supports asynchronous serial communication either to a USB host or to an
  RS232 DTE.
- __RAM__: I originally used a NEC D43256AC (32K by 8 bits) with a 100 ns
  access time. This proved to be a difficult component to acquire. I later
  selected the Fujitsu F84256-10LL simply because I could buy it on a weight
  basis! Any static 32 KB by 8 bits SRAM with an access time of 100 ns or less
  would do just as well.
- __CompactFlash__: CF is a fantastic technology that was not available in
  my Z80 days. It is quite easy to interface to an 8 bit data bus processor
  operating at 4 MHz. The medium itself includes an *Integrated Drive
  Electronics* (IDE) controller which is where all the intelligence resides.
  The original *Z79Forth Reference Board* did not have any support for mass
  storage as it was also missing from the Z80 incarnation. This shortcoming
  was ultimately fixed with release 2.x of the schematics.
- __Interrupt Support__: Again here, there is some historical background worth
  mentioning. The original board design (schematics 1.x) resorted to a stricly
  polling mechanism with respect to serial communication. Later on, after
  having experienced cut and paste difficulties, I felt that serial
  communication (input) ought to be interrupt driven. As a result, the current
  release trains (tags 2.x and 3.x) now program the ACIA so that it will
  assert <FIRQ#> when its receive data register is full.

  At a later stage, I recognized the fact that being able to dump the CPU
  registers on a manually triggered <NMI#> might be valuable. This is only
  needed in extremely dire situations and does not produce any symbolic
  information.

### Software Development
In 1984, I wrote a native Forth implementation for the TRS-80 model I
(Zilog Z80 based). That software was inspired by my understanding of the
79-STANDARD document, despite the fact that I did not have access to the
specification when I coded it. It survived in form of a printout that was
preciously stored in a trunk in my basement for some 35 years. Back in my Z80
days, mass storage was implemented via a cassette recorder, which meant that my
historic printout was very poorly commented. So the first order of business was
to properly document the Z80 implementation. Then came the porting effort
(starting with machine dependency handling), bug fixes, features addition
(`DOES>` pictured numeric output), 79-STANDARD compliance fixes, the
performance campaign and the required documentation production.

Unlike FORTH-83, 79-STANDARD does not specify the modulo operation in any
particular way. Compatibility concerns led me to move from the processor
native symmetric division to the more contemporary floored division.

At some point, I realized that most people coding in Forth nowadays resort
to the ANS94 specification. So I included ANS code porting guidelines and
ANS specific primitives. Later on in 2022 I went further by making the code
ANS94 compliant.

The resulting project delivery reflects this. The master branch has the
79-STANDARD implementation and the REL-ANS94 has the ANS flavor.

## Building the Platform
A kit is being put together. The estimated marker size is about 50 units. A PCB
is being designed but this is a slow process and financial constraints also
set an upper limit on how fast things can progress.

So, your best bet is to acquire all the parts yourself and wire wrap the system
as I did. A double Europe protyping card will be more than adequate. The bill
of material (see <HW/BOM.txt>) is believed to be accurate. Good luck!

## Repository Organization

<pre>
REL-ANS94
+-- HW                                       Hardware description files
|   +-- BOM.txt                              Bill of material
|   +-- circuit-20211015-1355.clkstretch.txt Clock stretcher simulation data
|   +-- J1-top-view.txt                      Pinout for the extension connector
|   +-- kicad
|   |   |-- <Kicad 5 Eeschema files>
|   +-- README-clkstretch.txt                How to use the clock stretcher sim.
|   +-- README.txt                           Proj. description from Elektor Labs
|   +-- Z79Forth-iteration2.2.2.pdf          Full PDF schematics
|   +-- Z79Forth-ww-botview.svg              Prototype sample layout
|   +-- Z79Forth-ww-topview.svg              Prototype sample layout
+-- KIMG0091.jpg
+-- LICENSE                                  The GPL version 3
+-- README.md                                This file
+-- SW                                       Software implementation
    |-- benchmarks
    |   |-- <Various benchmarks submitted to the VCFE in 2020>
    |   +-- vcfe2020.txt                     Benchmark results
    |-- doc
    |   |-- all-words.txt                    What is implemented and where
    |   |-- COMPILING.txt                    Must read before re-assembling!
    |   |-- DPANS94-conformance.txt          Mandatory ANS94 conformance doc.
    |   |-- ImplSpecMaterial.txt             Implementation specific doc.
    |   +-- LWVI-UserManual.txt              Documentation for the editor
    |-- examples
    |   |-- <Collection of example programs>
    |   +-- README.txt                       An overview of the examples
    |-- reference                            Reference material
    |   |-- FORTH-79.TXT                     The 79-STANDARD specification
    |   |-- FORTH-83.PRN                     The FORTH-83 specification
    |   |-- HD6309EP-TechRef.txt             Things to know about caching
    |   |-- HD63C09E.pdf                     CPU official datasheet from Hitachi
    |   +-- The_6309_Book.pdf                Chris Burke's invaluable book
    |-- src                                  Z79Forth source code
    |   |-- console.asm                      Serial console handling
    |   |-- constants.asm                    Check this file for system tunables
    |   |-- forth.asm                        Core Z79Forth implementation
    |   |-- forth.hex                        EEPROM Intel HEX image file
    |   |-- forth.lst                        An assembly listing
    |   |-- Makefile                         Software build driver
    |   |-- rtc.asm                          Not supported: MC146818 RTC
    |   |-- storage.asm                      CompactFlash storage handling
    |   +-- words.txt                        A list of supported words
    |-- testsuite
    |   |-- <A subset of the Forth2012 test suite>
    |   +-- README.txt
    +-- util
        |-- cfinit.sh                        Pointers to block numbers for apps
        |-- cfirq230122.img                  CompactFlash image (64 MB)
        |-- Makefile                         Generates txt2blk (host utility)
        +-- txt2blk.c                        txt2blk utility source code
</pre>

## See Also
[The Z79Forth Facebook Group](https://www.facebook.com/groups/505661250539263/)

[The Z79Forth Blog](https://z79forth.blogspot.com/)


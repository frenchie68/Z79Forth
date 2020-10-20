Forth like it's 1979 all over again!

This platform is designed as a basis for self-education and further hardware
development. The target Forth variant is the 79-STANDARD, an historic reference.
The whole design is based on the Hitachi HD63C09E--a much improved
implementation of the Motorola MC6809.

Main features are:

+ 5 MHz CPU operation.
+ 32 KB static RAM. Conceivably expandable to 48 KB.
+ 8 KB EEPROM running a native 79-STANDARD Forth sub-set implementation.
+ 6 spare IO device lines are decoded and available for further developments.
+ USB powered. The current consumption is about 50 mA.
+ Serial line console operating at 115200 bps.
+ Mass storage support on SanDisk CompactFlash (up to 64 MB).
+ Interrupt free design.

The software is licensed under the GNU General Public License version 3 and is
available at https://github.com/frenchie68/Z79Forth. Kicad schematics are also
provided over there.

Project status: working wire wrapped prototype. The software itself is believed
to be performing according to specifications. There are no known bugs at this
time.


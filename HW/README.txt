Forth like it's 1979 all over again!

This platform is designed as a basis for self-education and further hardware
development. The target Forth variant is the 79-STANDARD, an historic reference.
The whole design is based on the Hitachi HD63C09E--a much improved
implementation of the Motorola MC6809.

Main features are:

+ 4 MHz CPU operation.
+ 32 KB static RAM. Conceivably expandable to 48 KB.
+ 8 KB EEPROM running a native 79-STANDARD Forth sub-set implementation.
+ an extension connector that allows potential support for clock stretching.
  This is a must have feature when dealing with 1 MHz devices.
+ USB powered. The current consumption is about 65 mA.
+ Serial line console operating at 115200 or 38400 bps.
+ Interrupt driven serial communications. Both hardware and software flow
  control are required for optimal utilization.
+ Mass storage support on SanDisk CompactFlash (up to 64 MB).

The software is licensed under the GNU General Public License version 3 and is
available at https://github.com/frenchie68/Z79Forth. Kicad schematics are also
provided over there.

Project status:

- Working wire wrapped prototype.
- CPV has supplied working PCB design files for Kicad 6.
- A kit version of this board now is commercially available.
- The software itself is believed to be performing according to specifications.
  There are no known user visible bugs at this time.


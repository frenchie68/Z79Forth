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

- CPV has supplied working PCB design files for Kicad 6.
- A kit version of this board now is commercially available.
- The software itself is believed to be performing according to specifications.
  There are no known user visible bugs at this time.

The original wire wrapped prototype is described in Z79Forth-iteration2.2.2.pdf.
Released PCB versions so far are:

- 0.2: the illegitimate use of a 1 K resistor to drive the CPU RESET# input
  makes it necessary to have C1 as a 220 uF capacitor.
- 0.3: the RC circuitry for the CPU RESET# input has been fixed. As a result,
  C1 is 22 uF but an extra 10 K resistor (R6) is needed.
- 0.4: has a minor change meant to protect the EEPROM from being altered
  when the software goes beserk. Normally, the AT28C64B supports a software
  protection mechanism that relies on well known access patterns to enable
  or disable unwanted overwrites. A bug in my most recent of the TL866II+
  programming software causes the "protect afer programming" settings not to
  be persistent. To overcome this problem, PCB v0_4 connects the WE# (pin 27)
  of U10 to Vcc.

In the future, only PCB v0_4 will be shipped with the kit.

kikad5 files are of historical interest. The really interesting files reside
at HW/kikad6.



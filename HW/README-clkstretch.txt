Using signals provided over the J4 extension connector and the circuitry
described in circuit-20211015-1355.clkstretch.txt, it is possible to slow
down the CPU clock to 1 Mhz when slow devices are being accessed.

To view the CircuitJS simulation at work, start a browser and go to
https://www.falstad.com/ (with JavaScript support enabled!). Click on
the "Circuit Simulator" web link and then on "Full Screen Version."

In the menu at the top of the resulting page, select "File", "Open File"
and browse to the path where circuit-20211015-1355.clkstretch.txt lives.

Please note that, after a simulation reset, about 120 ns are necessary
for the circuit to be fully initialized. After that, the SLOW# push-button
can be used to initiate a slow bus cycle.

The signals drawn at the bottom of the screen are:

1	SYSCLK
2	E
3	Q
4	STRETCH#
5	VMA
 

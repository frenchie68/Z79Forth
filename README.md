# Z79Forth
This project has a hardware component and a software component.

- hardware: an Hitachi 63C09 based single board computer, aka the "Z79Forth
  Reference Board."
- software: a 79-STANDARD sub-set implementation entirely written in assembly
  language.

Additionally, some sample application programs and benchmarks are provided.
Please note that the GPL version 3 only applies to the assembly code. The
schematics and the Forth source code are released in the public domain.

"The 6309 Book" has been included in this distribution with the express assent
of its author, Chris Burke--may he be hereby thanked one more time for a great
piece of work, without which none of this would have been possible!

Known shortcomings of the current repository are as follows:

- unfinished editor (see SW/examples/lwvi.4th). Some extra work still is needed
  on the delete primitives.
  
November 4, 2021 notice: master is currently not entirely consistent. The
software delivered is ahead of the hardware documentation in that the software
assumes 2.2 schematics (interrupt based serial communications) and the hardware
documentation remains to be updated to reflect that basic fact. This is work
in progress. I will update master with the relevant hardware documentation as
soon as possible.

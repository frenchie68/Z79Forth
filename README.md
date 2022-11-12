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
  
Please note that it is not recommend to clone master directly, as the software
may not match the schematics. The recommended approach is to clone a release.

The master branch has the 79-STANDARD implementation. The REL-ANS94 branch
has a full Core word set implementation of the ANS 1994 standard specification.

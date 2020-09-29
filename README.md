# Z79Forth
This project has a hardware component and a software component.

- hardware: an Hitachi 63C09 based single board computer, aka the "reference Z79Forth board."
- software: a 79-STANDARD sub-set implementation entirely written in assembly language.

Additionally, some sample application programs and benchmarks are provided. Please note that
the GPL version 3 only applies to the assembly code. The schematics (which will be uploaded
later) and the Forth source code are released in the public domain.

"The 6309 Book" has been included in this distribution with the express assent of its author,
Chris Burke--may he hereby thanked one more time for a great piece of work, without which
none of this would have been possible!

Known shortcomings of the current repository are as follows:
- missing hardware schematics.
- unfinished editor (see examples/lwvi.4th). Some extra work still is needed on the delete
  primitives.
  

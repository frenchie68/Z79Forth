This directory contains the material for a ValgolI compiler generator.
It requires CompactFlash support and assumes that the CF image provided
in util/*img has been installed. In practice, the expected block setup
is:

600-602: Syntactic specification (grammar) of the target language.
603:     A sample program meant as a proof of concept.

Files in this directory:
valgol1.txt-64		The target language syntax specification.
valgoli.4th		The generated compiler (pretty printed).
valgol1ex.txt		The sample program expressed in the target language.
800257.808896.pdf	The original ACM reference paper.
meta2.4th		The compiler generator source code adapted for Z79Forth.

This example has not been blockified. Further instructions for use are provided
at the end of the meta2.4th file.


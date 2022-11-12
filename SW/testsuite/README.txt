This _is_ an ANS94 conformance validation test suite. It has been
tailored so as to be able to run it from blocks.

The original reference implemetation was borrowed from:
https://github.com/gerryjackson/forth2012-test-suite/tree/master/src

tester.4th has the basic testing infrastructure. It should be loaded
before any of core.4th, coreexttest.4th, blokcktest.4th, doubletest.4th
or stringtest.4th are.


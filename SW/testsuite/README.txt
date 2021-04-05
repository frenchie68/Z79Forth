Please note that even though this test suite is derived from the Forth2012
test suite, it has been modified to suit a validated Z79Forth implementation.
Therefore it should definitely not be used as a tool to assess ANSI conformance
status but rather as a non-regression tool.

The original reference implemetation was borrowed from:
https://github.com/gerryjackson/forth2012-test-suite/tree/master/src

tester.4th has the basic testing infrastructure. It should be loaded
before either core.4th or blokcktest.4th are.


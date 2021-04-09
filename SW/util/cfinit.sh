#!/bin/sh

./txt2blk -i ../examples/lwvi.4th -d /dev/sdb -s 16 \
-h '\ LWVI: a lightweight VI implementation. FLA Jan 21, 2020.'
# : viload 16 44 THRU ;

./txt2blk -i ../examples/life.4th -d /dev/sdb -s 50 \
-h '\ Game of Life - PEB'
# : lifeload 50 76 THRU ;

./txt2blk -i ../examples/dis.4th -d /dev/sdb -s 90 \
-h '\ Z79Forth 6309 disassembler. FLA, June 6, 2020.'
# : disload 90 95 THRU ;

./txt2blk -i ../examples/palflt.4th -d /dev/sdb -s 110 \
-h '\ Palindrome numbers filtering [10..50000]. FLA Dec 10, 2020'
# : palfltload 110 LOAD ;

./txt2blk -i ../examples/palgen.4th -d /dev/sdb -s 112 \
-h '\ Palindrome numbers generation. FLA Dec 10, 2020'
# : palgenload 112 LOAD ;

./txt2blk -i ../examples/hanoi.4th -d /dev/sdb -s 120 \
-h '( Towers of Hanoi. Original code by Peter Midnight.           )'
# : hanload 120 127 THRU ;

# [130..140[ used by the Forth2012 block word set test suite

./txt2blk -i ../examples/sapin.4th -d /dev/sdb -s 300 \
-h '\ Xmas ASCII art (Michel Jean).'
# : sapinload 300 LOAD ;

./txt2blk -i ../testsuite/tester.4th -d /dev/sdb -s 400 \
-h "\ Z79Forth test suite. Based on Gerry Jackson's Forth2012."
./txt2blk -i ../testsuite/core.4th -d /dev/sdb -s 405 \
-h "\ Z79Forth test suite. Based on Gerry Jackson's Forth2012."
./txt2blk -i ../testsuite/blocktest.4th -d /dev/sdb -s 466 \
-h "\ Z79Forth test suite. Based on Steve R. Palmer's Forth2012."

# : f12testload 400 404 THRU  \ Test infrastructure setup
#   405 465 THRU              \ Core word set test
#   466 504 THRU              \ Block word set test
# ;


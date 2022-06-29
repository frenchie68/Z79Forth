#!/bin/sh

# This script remains not entirely comprehensive in that it does not blank the
# target device or initialize block #1-6 at all.

CFDEV=/dev/sdb

if mount | cut -d\  -f1 | grep "^$CFDEV\$"; then
  echo "$0: $CFDEV is a mounted filesystem. Aborting."
  exit 1
fi

./txt2blk -i ../examples/lwvi.4th -d $CFDEV -s 16 \
-h '\ LWVI: a lightweight VI implementation. FLA Jan 21, 2020.'
# : viload 16 44 THRU ;

./txt2blk -i ../examples/life.4th -d $CFDEV -s 50 \
-h '\ Game of Life - Paul E. Bennett'
# : lifeload 50 76 THRU ;

./txt2blk -i ../examples/dis.4th -d $CFDEV -s 90 \
-h '\ Z79Forth 6309 disassembler. FLA, June 6, 2020.'
# : disload 90 94 THRU ;

./txt2blk -i ../examples/palflt.4th -d $CFDEV -s 110 \
-h '\ Palindrome numbers filtering [10..50000]. FLA Dec 10, 2020'
# : palfltload 110 111 THRU ;

./txt2blk -i ../examples/phrpal.4th -d $CFDEV -s 112 \
-h '\ Pablo Hugo Reda's code for palindrome numbers. May 29, 2021'
# : phrpalload 112 113 THRU ;

./txt2blk -i ../examples/hanoi.4th -d $CFDEV -s 120 \
-h '( Towers of Hanoi. Original code by Peter Midnight.           )'
# : hanload 120 127 THRU ;

# [130..140[ used by the Forth2012 block word set test suite

./txt2blk -i ../examples/dump.4th -d $CFDEV -s 150 \
-h '\ ASCII dump. FLA and PEB. Jan 4, 2022'
# : dumpload 150 154 THRU ;

./txt2blk -i ../examples/rtc.4th -d $CFDEV -s 200 \
-h '\ MC146818 RTC Support primitives. FLA. Nov 26,2021'
# : rtcload 200 205 THRU ;

./txt2blk -i ../examples/sapin.4th -d $CFDEV -s 300 \
-h '\ Xmas ASCII art (Michel Jean).'
# : sapinload 300 LOAD ;

./txt2blk -i ../examples/coop-mtask.4th -d $CFDEV -s 305 \
-h '\ Cooperative Multitasking, Matthias Koch. VD 2021/01.'
# : coopmtload 305 325 THRU ;

./txt2blk -i ../testsuite/tester.4th -d $CFDEV -s 400 \
-h "\ Z79Forth test suite. Based on Gerry Jackson's Forth2012."
./txt2blk -i ../testsuite/core.4th -d $CFDEV -s 405 \
-h "\ Z79Forth test suite. Based on Gerry Jackson's Forth2012."
./txt2blk -i ../testsuite/blocktest.4th -d $CFDEV -s 466 \
-h "\ Z79Forth test suite. Based on Steve R. Palmer's Forth2012."
./txt2blk -i ../testsuite/doubletest.4th -d $CFDEV -s 505 \
-h "\ Z79Forth test suite. Based on Hayes/Jackson's Forth2012."

# : f12testload 400 404 THRU  \ Test infrastructure setup
#   405 465 THRU              \ Core word set test
#   466 504 THRU              \ Block word set test
#   505 538 THRU              \ Double word set test
# ;

./txt2blk -i ../examples/expsys.4th -d $CFDEV -s 550 \
-h "\ Simple Forth expert system. Demitri Peynado: May 14, 2022."
# : expsysload 550 560 THRU ;


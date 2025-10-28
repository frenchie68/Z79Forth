#!/usr/bin/env python3

# Problem: patching the physical addresses in an Intel hex file results
# in checksums that need to be re-computed. This is precisely what this
# programs aims at doing.

import sys

# -----------------------------------------------------------------------------
def PrintNewChecksum(data):
  checksum = 0
  index = 0

  length = len(data) / 2
  while length > 0:
    # Iterate over all data bytes
    checksum += int(data[index:index+2], 16)
    index += 2
    length -= 1

  print('{:02X}'.format(-checksum & 0xFF))

# -----------------------------------------------------------------------------
def ParseIntelLine(line):

  # Remove EOL character and leading and trailing white space
  line = line.strip()

  if line[0:11] == ":00000001FF":
    # End of file marker found
    print(line[0:11])
    return True

  datalength = int(line[1:3], 16)
  checksum_index = (datalength + 4) * 2 + 1
  checksum = line[checksum_index:checksum_index + 2]
  address = int(line[3:7], 16)
  data = line[1:checksum_index]

  print(line[0:checksum_index], end='')

  # Print the recomputed the checksum
  PrintNewChecksum(data)

# -----------------------------------------------------------------------------

if len(sys.argv) != 2:
  sys.exit(1)
filename = sys.argv[1]

# ------------------------------------------------------------------------------
# process hex file

with open(filename) as fp:
  while True:
    line = fp.readline()
    if ParseIntelLine(line):
      break
fp.close()

sys.exit(0)


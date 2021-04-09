/*
 * Francois Laagel, Sun Mar 28 12:21:00 CEST 2021.
 * This is released in the public domain.
 *
 * txt2blk -i <inputFilename> -s <startingBlockNo> [-h <headerLine>]
 * [-d <device>]
 *
 * Convert a text input file to 1KB Forth block output. Please note that input
 * lines longer than 63 characters will be truncated. Should such a circumstance
 * occur, a warning would be issued to stderr.
 *
 * By default the converted output will be to files named block.nnnnnnnn--those
 * files are not supposed to already exist. If they do, the program will exit
 * under an error condition. This provides support for storage up to 64 MB
 * (65536 blocks). Otherwise the -d <device> option will direct this utility
 * to write directly to a disk device. Under Linux Mint, write access to disk
 * blocks can be enabled for the current user by resorting to:
 *
 * sudo usermod -a -G disk $LOGNAME
 *
 * Should you have to use to the above command, you would also have to restart
 * your session for the change to be effective. When writing directly to a disk
 * device, it is important that no underlying filesystem be mounted. This
 * program does in _no_ way enforce this! The target device should be listed
 * in /proc/partitions.
 *
 * To initialize a virgin CompactFlash card, the following Z79Forth code can be
 * used:
 *
 * : CFBLANK 0
 *   BEGIN
 *     DUP BUFFER 1024 BLANKS UPDATE
 *     1+ DUP 0=
 *   UNTIL DROP FLUSH ;
 *
 * Be aware that during the execution of CFBLANK, current consumption can rise
 * above 145 mA. If running from a USB power source, you should be OK since
 * the standard provides support for up to 200 mA.
 *
 * A prerequisite to being able to compile this program might be:
 *
 * sudo apt-get install glibc-headers
 *
 * Sample invokation:
 *
 * ./txt2blk -i ../examples/lwvi.4th -d /dev/sdb -s 16 \
 * -h '\ LWVI: a lightweight VI implementation. FLA Jan 21, 2020.'
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include <ctype.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define BLOCKSIZE	1024
#define MAXCOL		63   /* Discard chars aft. col #63 (starting from #1) */
#define MAXLNPERBLK	16

char	buffer[BLOCKSIZE], *blockPtr;
char	lineBuffer[256];

char	*SELF;
char	*inputFilename = NULL;
char	*headerLine = NULL, *headerFormatted;
int	startingBlockNo = 0, lastWrittenBlockNo;
int	currentBlockNo;
int	fileOutput = 1;
char	*deviceFilename = NULL;
FILE	*inputFile;

void
usage(void) {
  fprintf(stderr, "usage: %s -i <inputFilename>  -s <startingBlockNo> "
    "[-h <headerLine> [-d <device>]\n", SELF);
  exit(1);
}

FILE *
openInputFile(char *inFilename) {
  FILE	*rv;

  if((rv = fopen(inFilename, "r")) == NULL) {
    fprintf(stderr, "%s: cannot open %s\n", SELF, inFilename);
    exit(1);
  }

  return rv;
}

int
openOutputBlock(int blockNo) {
  char	outputFilename[32];
  int	fd;

  if(fileOutput) {
    sprintf(outputFilename, "block.%08d", blockNo);
    fd = open(outputFilename, O_CREAT|O_EXCL|O_DSYNC|O_WRONLY,
      S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);
  }
  else
    fd = open(deviceFilename, O_WRONLY|O_DSYNC);

  if(fd<0) {
    perror("open");
    exit(1);
  }

  /* Initialize the global block buffer pointer */
  blockPtr = buffer;

  return fd;
}

void
unlinkBlock(int blockNo) {
  char	outputFilename[32];

  if(fileOutput) {
    sprintf(outputFilename, "block.%08d", blockNo);
    unlink(outputFilename);
  }
  /* Nothing to do if the output is directly to disk */
}

void
emitLineToBuffer(char *addr) {
  memcpy(blockPtr, addr, 1 + MAXCOL);
  blockPtr += 1 + MAXCOL;
}

void
bufferToBlock(int fd, int blockNo) {
  if(!fileOutput)      /* We're writing directly to a disk device */
    lseek(fd, blockNo * BLOCKSIZE, SEEK_SET);
  (void)write(fd, buffer, BLOCKSIZE);    /* Flush the buffer */
}

void
emitBlankLines(int lineCountInBlock) {
  int	len;

  /* Blank lines until lineCountInBlock == MAXLNPERBLK */
  memset(lineBuffer, ' ', 1 + MAXCOL);
  for(; lineCountInBlock < MAXLNPERBLK; lineCountInBlock++)
    emitLineToBuffer(lineBuffer);
}

void
emitHeader(int *linesInBuffer) {
  *linesInBuffer = 0;
  if(headerLine) {
    emitLineToBuffer(headerFormatted);
    (*linesInBuffer)++;
  }
}

void
processInputFile(FILE *inFile) {
  int	lineCountInBlock, inputLineCount = 0, len, i, outFd;
  char	*continued;

  currentBlockNo = startingBlockNo;

  if(headerLine) {
    /* Format the header (capping its length to 63) to a (non-NUL terminated)
       64 character string whose last character is a space. */
    assert(headerFormatted = malloc(2 + MAXCOL));     /* MAXCOL + space + \0 */
    memset(headerFormatted, 0, 2 + MAXCOL);
    strncpy(headerFormatted, headerLine, MAXCOL);
    for(i = strlen(headerFormatted); i <= MAXCOL; i++)
      headerFormatted[i]=' ';
  }

  do {
    outFd = openOutputBlock(currentBlockNo);

    emitHeader(&lineCountInBlock);

    while((lineCountInBlock < MAXLNPERBLK) &&
      (continued = fgets(lineBuffer, sizeof(lineBuffer) - 2, inFile))) {
      char	*tempPtr;

      inputLineCount++;                             /* Global line count */

      /* Cap line lenght to MAXCOL+spc. Issue a warning if line is trunc'd */
      assert(tempPtr = strchr(lineBuffer, '\n')); /* CR must be in the input */
      len = tempPtr - lineBuffer;
      if(len > MAXCOL) {
        fprintf(stderr, "%s: WARNING: line %d truncated to %d columns.\n",
          SELF, inputLineCount, MAXCOL);
        len = MAXCOL;
      }
      while(len <= MAXCOL)
        lineBuffer[len++] = ' ';

      /* Emit formatted input line. */
      emitLineToBuffer(lineBuffer);
      lineCountInBlock++;
    }

    /* At this point, either we have a full buffer, or the input source file
       has been read entirely. */
    if((headerLine && lineCountInBlock == 1) || !lineCountInBlock) {
      /* If we get here, we are about to write an empty buffer (content wise) */
      assert(!continued);
      close(outFd);
      unlinkBlock(currentBlockNo);
      lastWrittenBlockNo = currentBlockNo - 1;
    }
    else {
      emitBlankLines(lineCountInBlock);
      bufferToBlock(outFd, currentBlockNo);
      close(outFd);
      lastWrittenBlockNo = currentBlockNo++;
    }
  } while(continued);

  printf("%d input lines processed\n", inputLineCount);
  printf("Blocks %d to %d written to %s\n", startingBlockNo,
    lastWrittenBlockNo, fileOutput ? "files" : "disk");

  printf("You may invoke this program with:\n");
  if(startingBlockNo == lastWrittenBlockNo)
    printf("%d LOAD\n", startingBlockNo);
  else
    printf("%d %d THRU\n", startingBlockNo, lastWrittenBlockNo);
}

int
main(int argc, char **argv) {
  int	opt;

  /* Command line parsing. */
  SELF = argv[0];
  while((opt = getopt(argc, argv, "i:h:s:d:")) != -1)
    switch(opt) {
      case 'i':
        inputFilename = optarg;
        break;
      case 'h':
        headerLine = optarg;
        break;
      case 's':
        startingBlockNo = atoi(optarg);
        break;
      case 'd':
        deviceFilename = optarg;
        fileOutput = 0;
        break;
      case '?':
        if(strchr("ihsd", optopt))
          fprintf(stderr, "%s: Option -%c requires an argument.\n", SELF,
            optopt);
        else if(isprint(optopt))
          fprintf(stderr, "%s: Unknown option `-%c'.\n", SELF, optopt);
        else
          fprintf(stderr, "%s: Unknown option character `\\x%x'.\n", SELF,
            optopt);
        return 1;
      default:
        usage();
    }

 /*
  * Sanity check:
  * - an input file name must be specified.
  * - startingBlockNo should be non-zero.
  */
  if(!inputFilename) {
    fprintf(stderr, "%s: an input filename must be specified (-i <fn>).\n",
      SELF);
    exit(1);
  }
  if(!startingBlockNo) {
    fprintf(stderr, "%s: a starting block number must be specified (-s <nb>) "
      "and cannot be zero.\n", SELF);
    exit(1);
  }

  assert(inputFile = openInputFile(inputFilename));
  processInputFile(inputFile);
  (void)fclose(inputFile);

  free(headerFormatted);

  return 0;
}


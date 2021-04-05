# In what follows, POSIX means POSIX/199309. The original incarnation.

all: txt2blk

clean:
	@rm -f txt2blk

txt2blk: txt2blk.c
	cc -o txt2blk -D__USE_POSIX txt2blk.c


LIBM=-lm
TFLAGS=-DUNIX
all: dhry whet linpack

clean:
	rm -f *.o dhry whet linpack

dhry: timers.o dhry21a.o dhry21b.o
	$(CC) $(CFLAGS) -o $@ $^

dhry21a.o: dhry21a.c
	$(CC) $(CFLAGS) -c dhry21a.c

dhry21b.o: dhry21b.c
	$(CC) $(CFLAGS) -c dhry21b.c

timers.o: timers.c
	$(CC) $(CFLAGS) $(TFLAGS) -c timers.c

whet: timers.o whet.o 
	$(CC) $(CFLAGS) -o $@ $^ $(LIBM)

whet.o: whet.c
	$(CC) $(CFLAGS) -c whet.c

linpack: linpack.o timers.o
	$(CC) $(CFLAGS) -o $@ $^

linpack.o: linpack.c
	$(CC) $(CFLAGS) -DDP -c linpack.c


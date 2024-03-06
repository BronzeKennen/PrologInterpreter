interpreter:
	ghc -o main main.hs
	ghc -o mgutests mgutests.hs

run:
	ghc -o main main.hs
	./main

all: run clean

.PHONY: clean

clean:
	rm main *.o *.hi
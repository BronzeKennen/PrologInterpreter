interpreter:
	ghc -o main main.hs

run:
	ghc -o main main.hs
	./main < testsimple.pl

all: run clean

.PHONY: clean

clean:
	rm main *.o *.hi
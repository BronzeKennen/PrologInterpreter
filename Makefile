interpreter:
	ghc -o main main.hs
	ghc -o mgutests mgutests.hs

clean:
	rm main mgutests *.o *.hi

.PHONY: clean interpreter

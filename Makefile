all: Automata.hs pt.hs ParseLib.hs
	ghc -o pt pt.hs

clean:
	rm pt *.hi *.o

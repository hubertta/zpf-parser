GHC = ghc
GHCFLAGS = -Wall -g -I. 

MODULES = main.hs test.hs LLLib.hs

all: test main

main: $(MODULES)
	$(GHC) --make main.hs -o $@ $(GHCFLAGS)

test: $(MODULES)
	$(GHC) --make test.hs -o $@ $(GHCFLAGS) -main-is Test

check: test
	./test

clean:
	rm -f *.o *.hi *.swp main test

.PHONY: main test

GHC = ghc
GHCFLAGS = -Wall

all: test main

main:
	$(GHC) --make main.hs -o $@ $(GHCFLAGS)

test:
	$(GHC) --make test.hs -o $@ $(GHCFLAGS) -main-is Test

check: test
	./test

clean:
	rm -f *.o *.hi *.swp main test

.PHONY: all main test check clean

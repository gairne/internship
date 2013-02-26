GHC=ghc

INCSUM_SRC = incsum.hs IncsumDPH.hs IncsumRepa.hs

GHC_FLAGS+=-odir obj -hidir obj
GHC_FLAGS+=-threaded -Odph -rtsopts
#GHC_FLAGS+=-eventlog
GHC_FLAGS+=-O2
GHC_FLAGS+= \
	-package dph-lifted-vseg-0.6.1.2

.PHONY : all
all : incsum

.PHONY : clean
clean :
	rm -rf obj incsum

incsum : $(INCSUM_SRC)
	$(GHC) $(GHC_FLAGS) --make $< -o $@

incsum.html : incsum
	./incsum -o $@

incsum-par.html : incsum
	./incsum -o $@ +RTS -N4

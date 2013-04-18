GHC=ghc

HSSRC = Main.hs Repa.hs DPH.hs

GHC_FLAGS+=-odir obj -hidir obj
GHC_FLAGS+=-threaded -Odph -rtsopts
#GHC_FLAGS+=-eventlog
GHC_FLAGS+=-O2
GHC_FLAGS+=-i../lib

.PHONY : all
all : Main Experimental

.PHONY : clean
clean :
	rm -rf obj Main Experimental

Main : $(HSSRC)
	$(GHC) $(GHC_FLAGS) -package repa-3.2.3.1 -hide-package repa-experimental-3.2.3.1 --make $< -o $@

Experimental : $(HSSRC)
	$(GHC) $(GHC_FLAGS) -package repa-experimental-3.2.3.1 -hide-package repa-3.2.3.1 --make $< -o $@

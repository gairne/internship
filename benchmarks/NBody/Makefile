GHC=ghc

HSSRC = MainBatch.hs

GHC_FLAGS+=-odir obj -hidir obj
GHC_FLAGS+=-threaded -Odph -rtsopts
#GHC_FLAGS+=-eventlog
GHC_FLAGS+=-O2
#GHC_FLAGS+= \
	-package dph-lifted-vseg-0.6.1.2
GHC_FLAGS+=-i../lib

.PHONY : all
all : Main

.PHONY : clean
clean :
	rm -rf obj Main

Main : $(HSSRC)
	$(GHC) $(GHC_FLAGS) --make $< -o $@


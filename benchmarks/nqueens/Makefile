GHC=~/ghcSrc/bin/ghc

HSSRC = Main.hs NqueensDPH.hs NqueensList.hs

GHC_FLAGS+=-odir obj -hidir obj
GHC_FLAGS+=-threaded -Odph -rtsopts
#GHC_FLAGS+=-eventlog
GHC_FLAGS+=-O3
#GHC_FLAGS+= \
	-package dph-lifted-vseg-0.6.1.2

.PHONY : all
all : nq

.PHONY : clean
clean :
	rm -rf obj nq

nq : $(HSSRC)
	$(GHC) $(GHC_FLAGS) --make $< -o $@


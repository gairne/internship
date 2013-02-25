GHC=ghc

HSSRC = Main.hs MstDph.hs MstList.hs 

GHC_FLAGS+=-odir obj -hidir obj
GHC_FLAGS+=-threaded -Odph -rtsopts
#GHC_FLAGS+=-eventlog
GHC_FLAGS+=-O2
#GHC_FLAGS+= \
	-package dph-lifted-vseg-0.6.1.2

.PHONY : all
all : mst

.PHONY : clean
clean :
	rm -rf obj mst

mst : $(HSSRC)
	$(GHC) $(GHC_FLAGS) --make $< -o $@


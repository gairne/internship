SUBS = dmm NBody nqueens quickhull quicksort
.PHONY: subdirs $(SUBS)

all: subdirs

subdirs: $(SUBS)
$(SUBS):
	$(MAKE) -C $@ all

%.h_: %.h
	cat ../../gnuc.h $< | cpp | grep -v '^#' >$@
	$(MZSCHEME) ../../cplt.ss < $@

clean:
	rm -f *~ *.h_


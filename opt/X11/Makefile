%.h_: %.h
	cat ../../gnuc.h $< | cpp | grep -v '^#' >$@
	mzscheme ../../cplt.ss < $@

clean:
	rm -f *~ *.h_


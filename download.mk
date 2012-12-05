# Given variable URLS, generate download rules and define a variable FILES.
FILES := $(notdir $(URLS))
UNPACKS := $(patsubst %.tar.gz,%/.unpack,$(FILES))

download: $(FILES)

download_clean:
	rm -f $(FILES)

%/.unpack: %.tar.gz
	tar xf $<
	touch $@

unpack: $(UNPACKS)
unpack_clean:
	rm -rf $(dir $(UNPACKS))

define download_template
$(1):
	wget --passive-ftp $(2)
endef
$(foreach url,$(URLS),$(eval $(call download_template,$(notdir $(url)),$(url))))



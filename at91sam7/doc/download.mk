# Given variable URLS, generate download rules and define a variable FILES.
FILES := $(notdir $(URLS))
download: $(FILES)
define download_template
$(1):
	wget $(2)
endef
$(foreach url,$(URLS),$(eval $(call download_template,$(notdir $(url)),$(url))))

download_clean:
	rm -f $(FILES)

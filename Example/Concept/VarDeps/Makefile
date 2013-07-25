IMPORTANTVARIABLE = a trouble

MD5SUM = md5sum

CHECKENV:
	$(MD5SUM) --version && touch $@

# Defines a function named GUARD which calculates md5 sum for its argument
# variable name. Note, that both cut and md5sum are members of coreutils
# package so they should be available on nearly all systems.
GUARD = $(1)_GUARD_$(shell echo $($(1)) | $(MD5SUM) | cut -d ' ' -f 1)

foo: CHECKENV bar $(call GUARD,IMPORTANTVARIABLE)
	@echo "Rebuilding foo with $(IMPORTANTVARIABLE)"
	@touch $@

$(call GUARD,IMPORTANTVARIABLE):
	@rm -f IMPORTANTVARIABLE*
	@touch $@


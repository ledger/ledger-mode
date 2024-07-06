EMACS ?= emacs
EMACS_FLAGS = --quick --directory . --directory ..
EMACS_BATCH = "$(EMACS)" --batch $(EMACS_FLAGS)
EMACS_INTERACTIVE = "$(EMACS)" $(EMACS_FLAGS)

EL  := $(wildcard *.el)
ELC := $(patsubst %.el,%.elc,$(EL))
ERT := $(filter-out test-helper.el,$(EL))

.PHONY: all
all: compile test

%.elc: %.el
	$(EMACS_BATCH) --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $<

.PHONY: test
test: compile
	$(EMACS_BATCH) $(addprefix --load ,$(ERT)) -f ert-run-tests-batch-and-exit

.PHONY: compile
compile: $(ELC)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: distclean
distclean: clean

.PHONY: checkdoc
checkdoc:
	make -C ../ lint-checkdoc

# Enables `make ledger-fontify/test-003`
define ERTDEFTEST
.PHONY: $(ERTDEF)
$(ERTDEF):
	$(EMACS_BATCH) --load $(ERTFILE) --eval "(ert-run-tests-batch-and-exit (quote $(ERTDEF)))"
endef
$(foreach ERTFILE,$(ERT),$(foreach ERTDEF,$(shell grep ert-deftest $(ERTFILE)|cut -d ' ' -f 2),$(eval $(ERTDEFTEST))))

EMACS ?= emacs
EMACS_FLAGS = --quick --directory . --directory ..
EMACS_BATCH = $(EMACS) --batch $(EMACS_FLAGS)
EMACS_INTERACTIVE = $(EMACS) $(EMACS_FLAGS)

EL  := $(wildcard *.el)
ELC := $(patsubst %.el,%.elc,$(EL))
ERT := $(filter-out test-helper.el,$(EL))

CHECKDOC_BATCH_EL := ../tools/checkdoc-batch.el

.PHONY: all
all: compile test

%.elc: %.el
	$(EMACS_BATCH) --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $<

.PHONY: test
test: test-interactive test-batch

.PHONY: test-interactive
test-interactive: compile
	$(EMACS_INTERACTIVE) $(addprefix --load ,$(ERT)) --eval "(ert-run-tests-interactively (quote (tag interactive)))"

.PHONY: test-batch
test-batch: compile
	$(EMACS_BATCH) $(addprefix --load ,$(ERT)) --eval "(ert-run-tests-batch-and-exit (quote (not (tag interactive))))"

.PHONY: compile
compile: $(ELC)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: distclean
distclean: clean
	rm -f $(CHECKDOC_BATCH_EL)

$(CHECKDOC_BATCH_EL):
	wget --no-verbose ftp://download.tuxfamily.org/user42/checkdoc-batch.el --output-document=$(CHECKDOC_BATCH_EL)

.PHONY: checkdoc
checkdoc: $(CHECKDOC_BATCH_EL)
	$(EMACS_BATCH) --load $(CHECKDOC_BATCH_EL) --funcall checkdoc-batch-commandline $(EL) | grep -E "el:[0-9]+:[0-9]+:" && exit 1 || exit 0

# Enables `make ledger-fontify/test-003`
define ERTDEFTEST
.PHONY: $(ERTDEF)
$(ERTDEF):
	$(EMACS_BATCH) --load $(ERTFILE) --eval "(ert-run-tests-batch-and-exit (quote $(ERTDEF)))"
endef
$(foreach ERTFILE,$(ERT),$(foreach ERTDEF,$(shell grep ert-deftest $(ERTFILE)|cut -d ' ' -f 2),$(eval $(ERTDEFTEST))))

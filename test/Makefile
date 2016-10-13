EMACS ?= emacs
EMACS_FLAGS = --quick --directory . --directory ..
EMACS_BATCH = $(EMACS) --batch $(EMACS_FLAGS)
EMACS_INTERACTIVE = $(EMACS) $(EMACS_FLAGS)

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
ERT := $(filter-out test-helper.el,$(wildcard *.el))

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

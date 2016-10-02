EMACS ?= emacs
EMACSFLAGS = --batch --quick --directory . --directory ..

ELC := $(patsubst %.el,%.elc,$(wildcard *.el))
ERT := $(filter-out test-helper.el,$(wildcard *.el))

.PHONY: all
all: compile test

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) --eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $<

.PHONY: test
test: compile
	$(EMACS) $(EMACSFLAGS) $(addprefix --load ,$(ERT)) --funcall ert-run-tests-batch-and-exit

.PHONY: compile
compile: $(ELC)

.PHONY: clean
clean:
	rm -f $(ELC)

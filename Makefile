RELEASE ?= 0
STACK := stack
STACK_TEST := $(STACK) build --bench --test --no-run-benchmarks
ifneq ($(RELEASE), 1)
  STACK_TEST += --fast
endif
STACK_BUILD := $(STACK_TEST) --no-run-tests
STACK_INSTALL := $(STACK_BUILD) --copy-bins
HASKELL_SOURCE_DIRECTORIES = $$(yq -r '.. | .["source-dirs"]? | select(. != null)' package.yaml)
HASKELL_SOURCE_FILES = $$(find $(HASKELL_SOURCE_DIRECTORIES) -name "*.hs")

.PHONY: install
install:
	$(STACK_INSTALL)

.PHONY: test
test:
	$(STACK_TEST)

.PHONY: ghcid
ghcid:
	$(STACK) exec --package ghcid -- ghcid

.PHONY: format
format:
	stack exec --package fourmolu -- fourmolu --mode inplace $(HASKELL_SOURCE_FILES)

.PHONY: check-format
check-format:
	stack exec --package fourmolu -- fourmolu --mode check $(HASKELL_SOURCE_FILES)

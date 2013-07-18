.NOTPARALLEL :

HLINT := hlint
CABAL ?= cabal

CABALFLAGS += --enable-tests
MAKEFLAGS += --warn-undefined-variables
.SECONDARY :

KITTEN = ./dist/build/Kitten/kitten
PRELUDE = ./dist/build/Kitten/prelude.ktn
TESTER = ./test/run.sh
TESTS = $(basename $(notdir $(wildcard test/*.ktn)))

TIME =
ifdef time
  TIME = time
  TIMEP = time -p
endif

.PHONY : default
default : build prelude unit test

.PHONY : all
all : deps configure default lint

.PHONY : build
build $(KITTEN) :
	$(CABAL) build

.PHONY : clean
clean :
	$(CABAL) clean
	rm -f test/*.actual

.PHONY : configure
configure :
	$(CABAL) configure $(CABALFLAGS)

.PHONY : deps
deps :
	$(CABAL) install $(CABALFLAGS) --only-dependencies

.PHONY : prelude
prelude : $(PRELUDE)

$(PRELUDE) : $(KITTEN) prelude.ktn
	cp prelude.ktn $(PRELUDE)
	$(KITTEN) --no-implicit-prelude $(PRELUDE)

.PHONY: unit
unit:
	@ $(TIMEP) $(CABAL) test

define TESTRULE
test-$1 : $(KITTEN) $(PRELUDE) $(TESTER)
	@ $(if ifdef time, printf "%-20s" "$1")
	@ $(TIME) $(TESTER) $$(realpath $(KITTEN)) $1
test : test-$1
endef

.PHONY : $(foreach TEST,$(TESTS),test-$(TEST))
$(foreach TEST,$(TESTS),$(eval $(call TESTRULE,$(TEST))))

.PHONY : lint
lint :
	@ $(TIMEP) $(HLINT) src lib

.PHONY : loc
loc :
	@ find src lib test \
		\( -name '*.hs' -o -name '*.ktn' \) \
		-exec wc -l {} + \
		| sort -n

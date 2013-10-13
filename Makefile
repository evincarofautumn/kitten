.NOTPARALLEL :

HLINT ?= hlint
CABAL ?= cabal

CABALFLAGS += --enable-tests
MAKEFLAGS += --warn-undefined-variables
.SECONDARY :

BUILDDIR = ./dist/build/kitten
EXAMPLES = $(wildcard examples/*.ktn)
KITTEN = $(BUILDDIR)/kitten
PRELUDE = $(BUILDDIR)/Prelude.ktn
TESTER = ./test/run.sh
TESTS = $(basename $(notdir $(wildcard test/*.ktn)))
YARN = $(BUILDDIR)/yarn

YARN_HEADERS = $(wildcard yarn/*.h)
YARN_SOURCES = $(wildcard yarn/*.cpp)

.PHONY : default
default : build prelude unit example test

.PHONY : all
all : deps configure default lint

.PHONY : build
build $(KITTEN) :
	$(CABAL) build

.PHONY : yarn
yarn : $(YARN) $(YARN_HEADERS)

$(YARN) : $(YARN_SOURCES)
	$(CXX) $^ -o $@ -std=c++11 -stdlib=libc++ -Wall -pedantic -g

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

$(PRELUDE) : $(KITTEN) lib/Prelude.ktn
	cp lib/Prelude.ktn $(PRELUDE)
	cp lib/Prelude_*.ktn $(BUILDDIR)
	$(KITTEN) --no-implicit-prelude $(PRELUDE)

.PHONY : unit
unit :
	$(CABAL) test

define EXAMPLE_RULE
example-$1 : $(KITTEN) $(PRELUDE)
	@$(KITTEN) --check "$1"
example : example-$1
endef

.PHONY : $(foreach EXAMPLE,$(EXAMPLES),example-$(EXAMPLE))
$(foreach EXAMPLE,$(EXAMPLES),$(eval $(call EXAMPLE_RULE,$(EXAMPLE))))

define TEST_RULE
test-$1 : $(KITTEN) $(PRELUDE) $(TESTER)
	@$(TESTER) $$(realpath $(KITTEN)) "$1"
endef

.PHONY : $(foreach TEST,$(TESTS),test-$(TEST))
$(foreach TEST,$(TESTS),$(eval $(call TEST_RULE,$(TEST))))

.PHONY : test
test : $(KITTEN) $(PRELUDE) $(TESTER)
	@$(TESTER) $(realpath $(KITTEN))

.PHONY : lint
lint :
	@ if which $(HLINT) 2>&1 >/dev/null; then \
	  $(HLINT) src lib; \
	else \
	  echo "No HLint found."; \
	fi

.PHONY : loc
loc :
	@ find src lib test \
		\( -name '*.hs' -o -name '*.ktn' \) \
		-exec wc -l {} + \
		| sort -n

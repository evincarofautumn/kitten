default :

HLINT ?= hlint
CABAL ?= cabal

CABALFLAGS += --enable-tests
MAKEFLAGS += --warn-undefined-variables
.SECONDARY :

BUILDDIR = ./dist/build/kitten
EXAMPLES = $(wildcard examples/*.ktn)
KITTEN = $(BUILDDIR)/kitten
PRELUDE = $(BUILDDIR)/Prelude.ktn
RUNTIME = kitten.o
TESTER = ./test/run.sh
TESTS = $(basename $(notdir $(wildcard test/*.ktn)))

PHONY_TARGETS = \
	deps \
	configure \
	build \
	unit \
	example \
	test \
	prelude \
	lint \
	loc \
	clean

# Declares a soft dependency such that, given:
#
# test :
# build :
# $(call SOFT_DEP_RULE_WITH,test,build,$(MAKECMDGOALS))
#
# 'test' will depend upon 'build' if 'test' is specified in
# the 'make' invocation.
#
# N.B. You must specify transitive dependencies.
define SOFT_DEP_RULE_WITH
$1 : | $(filter $2,$3)
endef

default_DEPS = build prelude unit example test
all_DEPS = deps configure $(default_DEPS) lint

BUILDING_PHONY_TARGETS = $(filter $(PHONY_TARGETS),$(MAKECMDGOALS))
ifeq ($(MAKECMDGOALS)$(filter-out default,$(MAKECMDGOALS)),)
BUILDING_PHONY_TARGETS += $(default_DEPS)
else
ifeq ($(filter-out all,$(MAKECMDGOALS)),)
BUILDING_PHONY_TARGETS += $(all_DEPS)
endif
endif

define SOFT_DEP_RULE
$(call SOFT_DEP_RULE_WITH,$1,$2,$(BUILDING_PHONY_TARGETS))
endef

# Soft dependencies between .PHONY targets.
deps_DEPS = clean
configure_DEPS = clean $(deps_DEPS) deps
build_DEPS = clean $(configure_DEPS) configure
unit_DEPS = clean $(build_DEPS) build
example_DEPS = clean $(build_DEPS) build $(prelude_DEPS) prelude
test_DEPS = clean $(build_DEPS) build $(prelude_DEPS) prelude
prelude_DEPS = clean $(build_DEPS) build
lint_DEPS = clean
loc_DEPS = clean
clean_DEPS =
$(foreach PHONY_TARGET,$(PHONY_TARGETS),$(eval $(call \
  SOFT_DEP_RULE,$(PHONY_TARGET),$($(PHONY_TARGET)_DEPS))))

.PHONY : default
default : $(default_DEPS)

.PHONY : all
all : $(all_DEPS)

.PHONY : build
build : $(KITTEN) $(RUNTIME)

.PHONY : $(KITTEN)
$(KITTEN) :
	$(CABAL) build
$(call SOFT_DEP_RULE,$(KITTEN),$(build_DEPS))

$(RUNTIME) :
	$(CC) -c kitten.c -I . -o $(RUNTIME) -Wall -Werror

.PHONY : clean
clean :
	$(CABAL) clean
	rm -f test/*.built
	rm -f test/*.err.c
	rm -f test/*.err.interpreted
	rm -f test/*.out.c
	rm -f test/*.out.interpreted

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
$(call SOFT_DEP_RULE,$(PRELUDE),$(prelude_DEPS))

.PHONY : unit
unit :
	$(CABAL) test

define EXAMPLE_RULE
example-$1 : $(KITTEN) $(PRELUDE)
	@$(KITTEN) --check "$1"
$(call SOFT_DEP_RULE,example-$1,$(example_DEPS))
example : example-$1
endef

.PHONY : $(foreach EXAMPLE,$(EXAMPLES),example-$(EXAMPLE))
$(foreach EXAMPLE,$(EXAMPLES),$(eval $(call EXAMPLE_RULE,$(EXAMPLE))))

define TEST_RULE
test-$1 : $(KITTEN) $(PRELUDE) $(TESTER)
	@$(TESTER) $$(realpath $(KITTEN)) "$1"
$(call SOFT_DEP_RULE,test-$1,$(test_DEPS))
test : test-$1
endef

.PHONY : $(foreach TEST,$(TESTS),test-$(TEST))
$(foreach TEST,$(TESTS),$(eval $(call TEST_RULE,$(TEST))))

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

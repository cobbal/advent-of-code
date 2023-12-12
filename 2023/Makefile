days := $(sort $(wildcard day-*))

OPAM := opam

all: $(days:day-%=%)

%: day-%
	@echo
	@echo "== $< =="
	eval $$($(OPAM) env); cd $<; dune exec $<
	@echo

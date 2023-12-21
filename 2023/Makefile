days := $(sort $(wildcard day-*))

OPAM := opam

all: $(days:day-%=%)

%: day-%
	@echo
	@echo "== $< =="
	eval $$($(OPAM) env); cd $<; OCAMLRUNPARAM="s=4G" dune exec $<
	@echo

time-all:
	dune build
	eval $$($(OPAM) env); \
	bash -c 'time (for day in $(days); do (cd $$day; OCAMLRUNPARAM="s=4G" ../_build/default/$$day/main.exe); done)'

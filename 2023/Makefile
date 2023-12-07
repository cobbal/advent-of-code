days := $(wildcard day-*)

all: $(days:day-%=%)

%: day-%
	@echo
	@echo "== $< =="
	cd $<; dune exec $<
	@echo

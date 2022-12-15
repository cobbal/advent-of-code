DAY=${shell basename $$PWD}

default:
	@cd ..; echo "Entering directory \`$$PWD'"
	mkdir -p build
	cd ..; PATH=/Users/acobb/.ghcup/bin/:$$PATH cabal exec ghc -- \
		-hidir ${DAY}/build -odir ${DAY}/build -o ${DAY}/build/day${DAY} \
		-O2 \
		${DAY}/*.hs
	time ./build/day${DAY}

profiling:
	@cd ..; echo "Entering directory \`$$PWD'"
	cd ..; cabal build --enable-profiling
	mkdir -p build
	cd ..; PATH=/Users/acobb/.ghcup/bin/:$$PATH cabal exec --enable-profiling ghc -- \
		-hidir ${DAY}/build -odir ${DAY}/build -o ${DAY}/build/day${DAY} \
		-O2 -prof -fprof-auto \
		${DAY}/*.hs
	time ./build/day${DAY} +RTS -p

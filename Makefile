.PHONY: bench

bench:
	cabal bench --benchmark-options='--output bench/crterion.html  +RTS -N' -j

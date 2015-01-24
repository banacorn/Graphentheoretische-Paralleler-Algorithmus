.PHONY: bench

bench:
	cabal bench -j --benchmark-options='--output bench/crterion.html +RTS -N'

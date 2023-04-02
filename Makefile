.PHONY: build clean repl watch doctest ;\
	cic ci formatc format lint lintc ;\
	hackage

# core

T = ""

repl:
	if [ -z "$(T)" ]; then \
		cabal repl; \
	else \
		cabal repl $(T); \
	fi

watch:
	ghcid --command "cabal repl $(T)"

doctest:
	cabal build --write-ghc-environment-files=always; \
	RUN_DOCTEST=1 cabal test doctest; \
	rm .ghc.environment.*

# ci

cic: formatc lintc

ci: lint format

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode check

format:
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt ;\
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode inplace

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.8#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.8#hlint

# generate dist and docs suitable for hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
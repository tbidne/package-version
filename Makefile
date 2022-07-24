# core

ARGS = ""

.PHONY: build
build:
	if [ -z "$(ARGS)" ]; then \
		cabal build; \
	else \
		cabal build $(ARGS); \
	fi

.PHONY: test
test:
	if [ -z "$(ARGS)" ]; then \
		RUN_DOCTEST=1 cabal test; \
	else \
		RUN_DOCTEST=1 cabal test $(ARGS); \
	fi

.PHONY: doctest
doctest:
	RUN_DOCTEST=1 cabal test doctest

.PHONY: unit
unit:
	cabal test unit

.PHONY: repl
repl:
	if [ -z "$(ARGS)" ]; then \
		cabal repl; \
	else \
		cabal repl $(ARGS); \
	fi

.PHONY: watch
watch:
	ghcid --command "cabal repl $(ARGS)"

# ci

.PHONY: cic
cic: formatc lintc haddockc

.PHONY: ci
ci: format lint

# formatting

.PHONY: formatc
formatc: cabalfmtc hsformatc nixpkgsfmtc

.PHONY: format
format: cabalfmt hsformat nixpkgsfmt

.PHONY: hsformat
hsformat:
	nix run github:tbidne/nix-hs-tools/0.6#ormolu -- --mode inplace

.PHONY: hsformatc
hsformatc:
	nix run github:tbidne/nix-hs-tools/0.6#ormolu -- --mode check

.PHONY: cabalfmt
cabalfmt:
	nix run github:tbidne/nix-hs-tools/0.6#cabal-fmt -- --inplace

.PHONY: cabalfmtc
cabalfmtc:
	nix run github:tbidne/nix-hs-tools/0.6#cabal-fmt -- --check

.PHONY: nixpkgsfmt
nixpkgsfmt:
	nix run github:tbidne/nix-hs-tools/0.6#nixpkgs-fmt

.PHONY: nixpkgsfmtc
nixpkgsfmtc:
	nix run github:tbidne/nix-hs-tools/0.6#nixpkgs-fmt -- --check

# linting

.PHONY: lint
lint:
	nix run github:tbidne/nix-hs-tools/0.6#hlint -- --refact

.PHONY: lintc
lintc:
	nix run github:tbidne/nix-hs-tools/0.6#hlint

.PHONY: haddockc
haddockc:
	nix run github:tbidne/nix-hs-tools/0.6#haddock-cov -- .

# generate dist and docs suitable for hackage
.PHONY: hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
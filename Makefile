.PHONY: help deps go pip apt

help:
	@echo "  deps        Install all dependencies."
	@echo "  apt         Install OS dependencies using apt-get."
	@echo "  pip         Install Python dependencies."
	@echo "  go          Install Go dependencies."

deps: apt pip go cabal haskell

go:
	go get -u github.com/nsf/gocode \
			  github.com/rogpeppe/godef \
			  github.com/golang/lint/golint

pip:
	easy_install pip && \
		pip install -r python_requirements.txt

apt:
	apt-get install \
		clang \
		shellcheck \
		xfonts-terminus \
		xfonts-terminus-oblique \
		ghc \
		cabal-install

cabal:
	cabal update
	cabal install cabal cabal-install

haskell:
	cabal install happy
	cabal install hasktags
	./cabal_clean
	cabal install stylish-haskell
	cabal install ghc-mod
	cabal install hoogle
	hoogle data
	./cabal_clean
	cabal install structured-haskell-mode

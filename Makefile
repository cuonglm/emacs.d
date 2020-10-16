.PHONY: help go pip apt cabal haskell

help:
	@echo "  apt         Install OS dependencies using apt-get."
	@echo "  pacman      Install OS dependencies using pacman."
	@echo "  pip         Install Python dependencies."
	@echo "  go          Install Go dependencies."
	@echo "  cabal       Update cabale to latest version."
	@echo "  haskell     Install Haskell dependencies."

go:
	go get golang.org/x/tools/gopls@latest

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
		cabal-install \
		npm \
		nodejs

pacman:
	pacman -S \
		clang \
		shellcheck \
		terminus-font \
		ghc \
		cabal-install \
		npm \
		nodejs

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
	./cabal_clean
	cabal install hindent

js:
	npm install -g tern

rust:
	cargo install racer rustfmt

CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/SystemF/Inference.hs : src/SystemF/Inference.ag 
	uuagc -dcfws -P src/SystemF src/SystemF/Inference.ag

haskell : src/SystemF/Inference.hs
	cabal install

.PHONY : haskell

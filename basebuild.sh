#!/bin/bash

cwd=$(pwd)

source base.sh

#run .  cabal_install --ghcjs ./codeworld-api
run .  cabal_install --ghcjs ./codeworld-base

run codeworld-base  cabal configure --ghcjs
run codeworld-base  cabal haddock --html
run codeworld-base  cabal haddock --hoogle

# Work-around for haddock dropping pattern synonyms in hoogle output.
grep -r -s -h 'pattern\s*[A-Za-z_0-9]*\s*::.*' codeworld-base/ \
    >> web/codeworld-base.txt

sh ./removeHasCallStack.sh

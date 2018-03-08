#!/bin/bash
# format the code according to some popular haskell formatting conventions
# stack build --copy-compiler-tool stylish-haskell
ghc_verion=8.2.2
bin_location=/home/osboxes/.stack/compiler-tools/x86_64-linux/ghc-${ghc_version}/bin
stylish_haskell_location=`stack exec -- which stylish-haskell ${bin_location}`

find . -name .stack-work -prune -o -type f -iname "*.hs" -print | xargs -I{} ${stylish_haskell_location} -i {} 

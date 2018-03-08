#!/bin/bash
# Run hlint over all haskell files in the project automatically applying suggestions
# to install hlint run:
# stack build --copy-compiler-tool hlint
# stack build --copy-compiler-tool apply-refact
ghc_verion=8.2.2
bin_location=/home/osboxes/.stack/compiler-tools/x86_64-linux/ghc-${ghc_version}/bin
hlint_location=`stack exec -- which hlint ${bin_location}`
find . -type f -iname "*.hs" | xargs -I{} ${hlint_location} {} --report --refactor --refactor-options=-i
${hlint_location} . --report

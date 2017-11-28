#!/bin/bash
# Run hlint over all haskell files in the project automatically applying suggestions
# to install hlint run:
# stack install hlint
# stack install apply-refactor
find . -type f -iname "*.hs" | xargs -I{} hlint {} --report --refactor --refactor-options=-i
hlint . --report

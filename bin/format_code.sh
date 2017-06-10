#!/bin/bash
# format the code according to some popular haskell formatting conventions
# stack install stylish-haskell
find . -type f -iname "*.hs" | xargs -I{} stylish-haskell -i {} 

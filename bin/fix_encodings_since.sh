#!/bin/bash
# Fix the encoding of all files in the current directory that have been modified in the last n days.

days=$1
find . -mtime -3 | tail -n +2 | xargs -I{} fix_encoding.sh {}

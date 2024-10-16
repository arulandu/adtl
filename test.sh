#!/bin/bash
rm -f ./tests/*.out

for inf in ./tests/*.adtl; do
  name=$(basename "$inf" .adtl)
  outf="./tests/${name}.out"
  dune exec -- adtl -n -l "$inf" > "$outf"
  echo "Running... $inf -> $outf"
done

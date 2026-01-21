#!/bin/bash

dune build --profile=release unikernel/main.exe
cp _build/solo5/unikernel/main.exe immuable.hvt
chmod +w immuable.hvt
strip immuable.hvt
solo5-hvt --mem=1024 --net:service=tap0 --block:immuable=pack.pack -- \
  immuable.hvt --ipv4=10.0.0.2/24 --color=always -vvv > log.txt

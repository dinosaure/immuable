#!/bin/bash

[ ! -d "bstr" ] && opam source bstr --dir bstr
[ ! -d "art" ] && opam source art --dir art
[ ! -d "carton" ] && opam source carton --dir carton
[ ! -d "digestif" ] && opam source digestif --dir digestif
[ ! -d "gmp" ] && opam source gmp --dir gmp
[ ! -d "h1" ] && opam source h1 --dir h1
[ ! -d "httpcats" ] && opam source httpcats --dir httpcats
[ ! -d "kdf" ] && opam source kdf --dir kdf
[ ! -d "mhttp" ] && opam source mhttp --dir mhttp
[ ! -d "mirage-crypto-rng-mkernel" ] && opam source mirage-crypto-rng-mkernel --dir mirage-crypto-rng-mkernel
[ ! -d "mkernel" ] && opam source mkernel --dir mkernel
[ ! -d "mnet" ] && opam source mnet --dir mnet
[ ! -d "tls" ] && opam source tls --dir tls
[ ! -d "vifu" ] && opam source vifu --dir vifu
[ ! -d "x509" ] && opam source x509 --dir x509

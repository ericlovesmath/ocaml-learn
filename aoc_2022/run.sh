#!/bin/bash

dune build
dune exec "_build/default/bin/day$1.exe"

#!/bin/bash

for f in test/*.sld;
do
    chibi-scheme -Isrc $f
done

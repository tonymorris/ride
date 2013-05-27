#!/bin/sh

rm -rf _cache
ghc --make -outputdir /tmp/ site.hs
./site preview

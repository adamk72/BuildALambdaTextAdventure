#!/bin/zsh

# ormolu --mode inplace $(find ./src -name '*.hs')
# ormolu --mode inplace $(find ./app -name '*.hs')

stylish-haskell -i **/*.hs 2>/dev/null

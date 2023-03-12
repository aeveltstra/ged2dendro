# Compiles ged2dendro from scratch using ghc.
# Assumes all dependencies already got downloaded and are accessible.
#
# Requires:
# - GHC 8.8.3 or newer.

ghc --make -package containers -package text -O2 -o ./bin/ged2dendro-1.1.0.5 ./Main.hs

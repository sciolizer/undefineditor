rm *.tix
ghc -package ghc -fhpc --make TestCollectBindings.hs &&
./TestCollectBindings &&
hpc markup TestCollectBindings &&
echo success

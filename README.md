Learning Haskell on my own following the outline from [http://www.scs.stanford.edu/11au-cs240h](http://www.scs.stanford.edu/11au-cs240h).

This program is for [Lab2](http://www.scs.stanford.edu/11au-cs240h/labs/lab2.html).

    cabal configure --enable-tests
    cabal build
    cabal test
    ./dist/build/rtree/rtree data/rects.txt
  
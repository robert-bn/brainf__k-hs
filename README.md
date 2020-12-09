# brainf__k-hs
Brainf__k-hs is a [brainf__k](https://en.wikipedia.org/wiki/Brainfuck) interpreter written in Haskell.

## Build
Clone the git repository and build using GHC. I have tested it on GHC version 8.8.4, but newer versions may also work.
```
git clone https://github.com/robert-bn/brainf__k-hs
cd brainf__k-hs/
ghc --make brainf__k.hs -o bf -O2
```
You may need to install some dependencies using cabal to build successfully. Brainfuck-hs requires the following packages:
1. mtl
2. monad-loops

## Usage
Call `bf` with a brainf__k source file as its only argument.

### Example
```
wget "https://copy.sh/brainfuck/prog/hanoi.bf"
./bf hanoi.bf
```

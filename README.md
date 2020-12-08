## brainf__k-hs
A [brainf__k](https://en.wikipedia.org/wiki/Brainfuck) interpreter written in Haskell

## Build
Clone the git repository and build using GHC.
```
git clone https://github.com/robert-bn/brainf__k-hs
cd brainf__k-hs/
ghc --make brainf__k.hs -o bf -O2
```

## Usage
Call `bf` with a brainf__k source file as its only argument.

# Example
```
wget "https://copy.sh/brainfuck/prog/hanoi.bf"
./bf hanoi.bf
```

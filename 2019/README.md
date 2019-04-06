# 2019

## Setup

Setup Rust 1.24.1 by executing the following from this directory:
```
$ rustup install 1.24.1
$ rustup override set 1.24.1
```

## Compiling

These are the language specific commands used to compile/run a solution:

* Python - 3.5.3
```sh
$ python3 solution.py
```
* Rust - 1.24.1
```sh
$ rustc -C opt-level=3 -o 1-solution 1-name-of-problem.rs
$ ./1-solution
```
* Haskell - 8.0.1
```sh
$ ghc +RTS -V0 -RTS -rtsopts -O2 -o 1-solution 1-name-of-problem.hs
$ ./1-solution +RTS -V0 -RTS
```


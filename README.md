# Haskell and Stack Intro

## Stack quick start

https://docs.haskellstack.org/en/stable/README/
https://docs.haskellstack.org/en/stable/GUIDE/

```bash
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe

stack ghci
stack clean
```

## Components

The Glasgow Haskell Compiler (GHC), the premier Haskell compiler. Stack will manage your GHC installations and automatically select the appropriate compiler version for your project.

The Cabal build system, a specification for defining Haskell packages, together with a library for performing builds.

The Hackage package repository, providing more than ten thousand open source libraries and applications to help you get your work done.

The Stackage package collection, a curated set of packages from Hackage which are regularly tested for compatibility. Stack defaults to using Stackage package sets to avoid dependency problems.

## Files

`stack setup` downloads ghc compilers and such.
All of stack stuff lives in `~/.stack/` (global stuff including ghc) and local project `./stack-work/` (like built executables and snapshots)

`helloworld.cabal` file is updated automatically by build command

The `Setup.hs` file is a component of the Cabal build system which stack uses. It's technically not needed by stack, but it is still considered good practice in the Haskell world to include it

`stack.yaml` file, which gives our project-level settings. `packages` setting usually only `.` unless multi package project.
`resolver` setting is the ghc version to use, version of package dependencies etc.
use the `extra-deps` field in stack.yaml to define extra dependencies not present in the resolver

`package.yaml` generates `.cabal` file

In Cabal, we have individual packages, each of which contains a single .cabal file. The .cabal file can define 1 or more components: a library, executables, test suites, and benchmarks. It also specifies additional information such as library dependencies, default language pragmas, and so on.

Add dependencies to `package.yaml`


## Misc

```bash
stack exec -- which ghc
```

Package sets are also known as snapshots.

On https://www.stackage.org/lts , we can find

- The appropriate resolver value (resolver: lts-14.27, as is currently the latest LTS)
- The GHC version used
- A full list of all packages available in this snapshot
- The ability to perform a Hoogle search on the packages in this snapshot
- A list of all modules in a snapshot, which can be useful when trying to determine which package to add to your  package.yaml file.


`stack unpack yackage-0.8.0` to get the code.
`stack init` to add `stack.yaml` to the unpacked project by parsing all .cabal files.


 A database consists of a GHC package database (which contains the compiled version of a library), executables, and a few other things as well.

Databases in stack are layered. For example, the database listing we just gave is called a local database. That is layered on top of a snapshot database, which contains the libraries and executables specified in the snapshot itself. Finally, GHC itself ships with a number of libraries and executables, which forms the global database

Haddock is haskell documentation.


```
build    Build the package(s) in this directory/configuration
install  Shortcut for 'build --copy-bins'
test     Shortcut for 'build --test'
bench    Shortcut for 'build --bench' (benchmarks)
haddock  Shortcut for 'build --haddock'
```

The install command does precisely one thing in addition to the build command: it copies any generated executables to the local bin path, so you can execute it directly without having to use stack exec

Haskell is a compiled language and in GHC gets compiled to C and then to object code/binary.

```
.hs
A Haskell module.
.lhs
A “literate Haskell” module.
.hspp
A file created by the preprocessor.
.hi
A Haskell interface file, probably compiler-generated.
.hie
An extended Haskell interface file, produced by the Haskell compiler.
.hc
Intermediate C file produced by the Haskell compiler.
.c
A C file not produced by the Haskell compiler.
.ll
An llvm-intermediate-language source file, usually produced by the compiler.
.bc
An llvm-intermediate-language bitcode file, usually produced by the compiler.
.s
An assembly-language source file, usually produced by the compiler.
.o
An object file, produced by an assembler.
```

if a Haskell source file deliberately uses name shadowing, it should be compiled with the -Wno-name-shadowing option. Rather than maintaining the list of per-file options in a Makefile, it is possible to do this directly in the source file using the OPTIONS_GHC pragma

GHCI - GHC's interactive runtime environment
More details : https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci


## Elm bridge

`elm-bridge` is a Haskell module.

Building the bridge from Haskell to Elm and back. Define types once, and derive the aeson and elm functions at the same time, using any aeson option you like.

Basically write the type in Haskell and get autogenerated Elm file with the same/corresponding type and helper functions for encoding and decoding json .

`aeson` is a Haskell json library.

## Haskell

To learn basics of haskell including syntax, functions, types, type classes, etc., refer to [src/Lib.hs](src/Lib.hs)

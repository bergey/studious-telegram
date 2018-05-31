# haskell-builds

Example of building the same project with multiple options.

## Goals

Ideally, a build system should be:

- reproducible
- fast to build
- support remote, parallel, maybe even distributed builds
- handle non-Haskell dependencies, at least in the final Docker image

Of the options available for Haskell, are some better at achieving this?

## Project

All examples use the `yesod-postgres` sample project from `stack templates`.  I hope that this is small enough to build quickly, but includes enough dependencies to exercises the build systems.  Postgres is my strong preference for real work, and pulls in some non-Haskell dependencies.

## Build Systems

- cabal sandbox
- cabal new-build
- stack
- nix (more details?)
- bazel

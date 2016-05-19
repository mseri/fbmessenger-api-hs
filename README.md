# FBMessenger API

High-level bindings to the Messenger Platform API based on [servant](https://haskell-servant.github.io/) library. We try to maintain the structure compatible with [telegram-api](https://github.com/klappvisor/haskell-telegram-api).

# Usage

... todo ...

# Contribution

1. Fork repository
2. Do some changes
3. Create pull request
4. Wait for CI build and review

You can use stack to build project

    stack build

To run test you have to create your own bot. Keep your access token safe!

    stack test --test-arguments "$BOT_TOKEN"

_(this will need review)_

# TODO

- Still everything!
- Set up Travis or whatever for testing
- Remove `servant-server`, `wai`, `warp` from the dependencies (both from the cabal file and `stack.yaml`. Now they are used only for compiling `Lib.hs` that is just a stub to use as playground.

# haskoin-script

Implementation of Bitcoin script parsing and evaluation

Project Status: **Experimental**

## Description

**haskoin-script** is a component of **haskoin**, an ecosystem of haskell
libraries implementing the various parts of the bitcoin protocol. This library
provides all the functionality for parsing standard script formats and
evaluating arbitrary scripts.

## Synopsis

```haskell
    -- todo ...
```

## Usage

All the types and functions in this section are exported by `Haskoin.Script`

```haskell
    import Haskoin.Script
```
Todo ...

## Dependencies

- Cabal package manager

```sh
    # in Ubuntu
    apt-get install cabal-install
```

- haskoin-util

```sh
    # haskoin-util is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-util.git
    cd haskoin-util
    cabal install
```

- haskoin-crypto

```sh
    # haskoin-crypto is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-crypto.git
    cd haskoin-crypto
    cabal install
```

- haskoin-protocol

```sh
    # haskoin-protocol is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-protocol.git
    cd haskoin-protocol
    cabal install
```

## Installing

```sh
    # haskoin-script is not on Hackage (yet) 
    git clone https://github.com/plaprade/haskoin-script.git
    cd haskoin-script
    cabal install
```

### Tests

If you are missing the test dependencies:

```sh
    cabal install --enable-tests
    cabal test
```

If you have the test dependencies, you can build without installing:

```sh
    cabal configure --enable-tests
    cabal build
    cabal test
```

The tests can take a few minutes to run.

## Bugs

Please report any bugs in the projects bug tracker:

[github.com/plaprade/haskoin-script/issues](http://github.com/plaprade/haskoin-script/issues)

## Contributing

We're glad you want to contribute! It's simple:

- Fork haskoin-script
- Create a branch `git checkout -b my_branch`
- Commit your changes `git commit -am 'comments'`
- Push the branch `git push origin my_branch`
- Open a pull request

Code guidelines:

- 80 columns.
- 4 space indentation. No tabs.
- Follow the general style of the code, whenever it makes sense.

## Supporting

You can support the project by donating in [Bitcoins](http://www.bitcoin.org)
to:

**176CwMCWMq1y9CxFZWk7Vfoka5PoaNzxRq**


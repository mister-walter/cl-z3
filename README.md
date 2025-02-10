# Lisp-Z3 Interface

This library provides an interface through which one can use the
[Z3](https://github.com/Z3Prover/z3/) Satisfiability Modulo Theories
(SMT) solver from inside of Common Lisp. In short, this interface
allows one to leverage Z3's ability to solve constraints involving
variables over numbers, strings, bitvectors, uninterpreted functions,
and more, including optimization problems.

## Setup

### Prerequisites
- A Common Lisp implementation (tested on SBCL, may work on other Lisps)
- Quicklisp
- Z3 (see below)

### Installing Z3
To use this library, you first need to install Z3 onto your
system. Many package managers offer a prepackaged version of Z3, so it
is likely easiest to install Z3 using your package manager rather than
building it from source. If you're on macOS, Homebrew provides
prebuilt Z3 packages as well.

Depending on your operating system, you may also need to install
a "z3-dev" package. On Ubuntu, this package is called `libz3-dev`.

You will also need a working C compiler to use the interface. On
Ubuntu, the `build-essential` package should include everything you
need, though it is fairly large and contains some unneeded
software. One could also try just installing `gcc` or `clang`.

After getting Z3 installed, you should be able to run it through the
command line. To test this, execute `z3 --version` in your terminal
and verify that it reports something along the lines of `Z3 version
4.13.4 - 64 bit` (your version or architecture may be different,
that's OK).



### Installing the interface
The easiest way to install and use this library is to clone this
repository inside of your Quicklisp `local-projects` directory, which
typically is located at `~/quicklisp/local-projects`. I will refer to
this directory as <ql-local-projects> below.

To test that everything has been installed properly, start SBCL inside
the `<ql-local-projects>/cl-z3/examples/` directory and run the
commands inside of the `<ql-local-projects>/cl-z3/examples/basic.lisp`
file. If no errors occur, then you're all set.

## Usage

If the library was successfully installed, you should be able to load
it from Lisp using the following code, assuming that Quicklisp is
already loaded:
```lisp
(ql:register-local-projects)
(ql:quickload :cl-z3)
```

The examples should provide a fairly good overview of various features
of the library, but the `operators.md` file inside of this directory
contains additional information about the operators we support
converting to Z3 statements.

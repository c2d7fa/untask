# Untask

## Dependencies

Untask requires Racket version 7.4 or later. It also requires `rlwrap` to be
installed in order to run.

### Ubuntu

On Ubuntu, use the PPA `plt/racket` to get the latest version of Racket:

    sudo apt-add-repository ppa:plt/racket
    sudo apt-get install racket

You will also have to install `rlwrap`. This can be done with:

    sudo apt-get install rlwrap

### Arch Linux

Install Racket and `rlwrap` with:

    sudo pacman -S racket rlwrap

### Windows

Windows is not supported.

### MacOS

MacOS is not supported.

## Building

First, install the necessary dependencies (see above).

Clone this repository into a directory named `untask` (the name matters for
running tests).

In the top-level directory, run:

    raco pkg install --auto

This will install any missing dependencies. In order to create an executable, run:

    make untask

This will create the file `untask`, which can be run directly.

[![Travis build status](https://travis-ci.org/fmlove/tracerutils.svg?branch=master)](https://travis-ci.org/fmlove/tracerutils)

# tracerutils
Functions for use with [rcatmaid](https://github.com/jefferis/rcatmaid), [nat](https://github.com/jefferis/nat), and [elmr](https://github.com/jefferis/elmr).  Intended to provide some shortcuts for common tasks like plotting neurons from CATMAID, running NBLAST, etc.

## Installation
    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("fmlove/tracerutils")
    library(tracerutils)

The package is currently only compatible with macOS.

## Development
This package is very much a work in progress.  It is largely untested, has a few known issues, and is likely to change significantly over the short to medium term.  I would recommend that you don't make anything critical dependent on **tracerutils** until it is in a more stable state, but it might be useful in your day-to-day work.

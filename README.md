# tracerutils
Functions for use with [rcatmaid](https://github.com/jefferis/rcatmaid), [nat](https://github.com/jefferis/nat), and [elmr](https://github.com/jefferis/elmr).  Intended to provide some shortcuts for common tasks like plotting neurons from CATMAID, running NBLAST, etc.

## Installation
    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("fmlove/tracerutils")
    library(tracerutils)

The package is currently only compatible with macOS.

## Development
This package is very much a work in progress.  It is largely untested, has quite a few known issues, and is likely to change significantly over the short term.  I would recommend that you don't make anything dependent on **tracerutils** until it is in a more stable state.

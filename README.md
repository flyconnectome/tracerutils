[![Travis build status](https://travis-ci.org/fmlove/tracerutils.svg?branch=master)](https://travis-ci.org/fmlove/tracerutils)

# tracerutils
Functions for use with [rcatmaid](https://github.com/jefferis/rcatmaid), [nat](https://github.com/jefferis/nat), and [elmr](https://github.com/jefferis/elmr).  Intended to provide some shortcuts for common tasks like plotting neurons from CATMAID, sampling synapses, etc.

## Installation
    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("fmlove/tracerutils")
    library(tracerutils)

The package has only been tested on macOS.

## Development
This package is very much a work in progress.  I will do my best to keep all changes backwards-compatible, but I would recommend that you don't make anything critical dependent on **tracerutils** until it is in a more stable state.

If there are any features you would like to see in **tracerutils**, please let me know!  Planned improvements will be listed on the [issues](https://github.com/fmlove/tracerutils/issues) page, and you can submit your suggestions there.

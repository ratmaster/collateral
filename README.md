## collateral: Reproducible, comfortable and fast processing with R
[![Package-License](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT)

### Introduction

`collateral` provides you with functions you might miss in R, such as

* `p*apply()` functions for reproducible parallel processing.
* `%onerror%` operators for handling errors more direct.

However, this package is a proof of concept and please use it, at the moment, **for testing purposes only**. More detailed information will be provided soon.

### Installation

Up to now, `collateral` is not available on CRAN because it needs some more tests for errors to provide a high level of stability. However, you might install a testing version:

* Install the most recent version:

    ```r
    devtools::install_github("ratmaster/collateral")
    ```
    
* When the installation is finished, you can employ the package:

    ```r
    library(collateral)
    ```
    
* If you find any bugs or if you have some ideas of new features, please 
  consider creating an Github issue or contacting me directly by mail.


### Author

Fabian Raters.


### License

MIT, 2016-2017.

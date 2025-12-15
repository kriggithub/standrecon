
<!-- README.md is generated from README.Rmd. Please edit that file -->

# standrecon: Reconstruct Forest Stand Conditions

<!-- badges: start -->

<!-- badges: end -->

The `standrecon` R package was created to summarize reference historical
stand conditions by total basal area and tree density using contemporary
field data.

standrecon automates the methods described for forest stand
reconstruction by Sakulich & Scholl, 2005, using dead tree decomposition
rates as per Fule, 1993.

Data sets which use default species codes (such as PIPO or PSME) are
able to utilize the default bark correction, which otherwise must be
implemented manually.

## Installation

You can install the development version of standrecon from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("kriggithub/standrecon")
```

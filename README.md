
<!-- README.md is generated from README.Rmd. Please edit that file -->

# standrecon: Reconstruct Forest Stand Conditions

<!-- badges: start -->

[![R-CMD-check](https://github.com/kriggithub/standrecon/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kriggithub/standrecon/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `standrecon` R package was created to summarize reference historical
stand conditions by total basal area and tree density using contemporary
field data.

The package implements a workflow for forest stand reconstruction using
tree mortality information derived from decay classes and
species-specific growth rates, following approaches described in the
forest ecology literature.

Data sets using standard species codes (such as PIPO or PSME) can use
built-in bark correction equations; custom equations may be supplied for
other species.

## Installation

You can install standrecon from CRAN with

``` r
install.packages("standrecon")
```

Alternatively, can install the development version of standrecon from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("kriggithub/standrecon")
```

## Example

The example below demonstrates how to reconstruct forest stand basal
area and stem density at historical reference years using tree-level
inventory data.

``` r
library(standrecon)

# Load example data included with the package
data(standrecon_example_data)

# Species-specific average radial growth (mm/year)
avg_inc <- c(
  PIEN = 1.5,
  ABBI = 1.3,
  PIPO = 1.4
)

# Reconstruct stand conditions
out <- standrecon(
  data = standrecon_example_data,
  meas_year = 2025,
  ref_year = c(1950, 1975),
  avg_inc_vec = avg_inc,
  plot_size = 1000
)

# View the first few rows of output (percentiles for sensitivity analysis)
head(out)
#>            type year percentile species basal_area stem_density
#> 1 reconstructed 1950       0.25    ABBI   9.266339          400
#> 2 reconstructed 1950       0.25    PIEN   9.692050          430
#> 3 reconstructed 1950       0.25    PIPO   7.605282          250
#> 4 reconstructed 1950       0.50    ABBI   7.151095          370
#> 5 reconstructed 1950       0.50    PIEN   7.287843          410
#> 6 reconstructed 1950       0.50    PIPO   5.500327          240
```

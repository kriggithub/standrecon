
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

You can install the development version of standrecon from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("kriggithub/standrecon")
```

Or, you can download it from CRAN with

``` r
install.packages("standrecon")
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
  PIEN = 0.5,
  ABBI = 0.3,
  PIPO = 0.4
)

# Reconstruct stand conditions
out <- standrecon(
  data = standrecon_example_data,
  meas_year = 2025,
  ref_year = c(1950, 1975),
  avg_inc_vec = avg_inc,
  plot_size = 1000
)

# View the first few rows of output
head(out)
#>            type year percentile species basal_area stem_density
#> 1 reconstructed 1950       0.25    ABBI   26.92181          530
#> 2 reconstructed 1950       0.25    PIEN   30.76367          730
#> 3 reconstructed 1950       0.25    PIPO   18.15475          310
#> 4 reconstructed 1950       0.50    ABBI   26.26240          520
#> 5 reconstructed 1950       0.50    PIEN   29.70578          730
#> 6 reconstructed 1950       0.50    PIPO   17.41799          310
```

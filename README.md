
# fars

<!-- badges: start -->
<!-- badges: end -->

The goal of FARS is to provide a set of tools to read, analyze and visualize
data acquired from the databases of the US National Highway Traffic Safety
Administration's Fatality Analysis Reporting System (FARS).

## Installation

You can install the released version of fars from [GitHub](https://github.com/doctormanuel/fars) with:

``` r
library(devtools)
install.packages("doctormanuel/fars")
```

## Example

The following example will provide a summary of the FARS data for every month
in 2013-2015.

``` r
library(fars)
fars_summarize_years(years = c(2013:2015))
```

The following example will provide a map with plotted FARS data for the state of
Florida (state number 12) in 2015.

``` r
library(fars)
fars_map_state(state.num = 12, year = 2015)
```

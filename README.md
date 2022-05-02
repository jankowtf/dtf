
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dti

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/valid)](https://CRAN.R-project.org/package=valid)
<!-- badges: end -->

## Installation

You can install the development version of from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rappster/dti")
```

## What?

## Why?

## How?

``` r
library(dti)
```

### Bundle `AutoFill`

#### Default

See <https://rstudio.github.io/DT/extensions.html> (setion *1.
AutoFill*)

With the AutoFill extension, you will see a blue square in the
bottom-right corner of a cell when you mouse over the cell. You can drag
it to automatically fill the column.

``` r
mtcars %>% datatable2(bundle = dt_bundle_autofill())
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
mtcars %>% datatable2(bundle = "AutoFill")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

#### Custom options

For comparison when calling `DT::datatable()`

``` r
mtcars %>% DT::datatable(
    extensions = "AutoFill", 
    options = list(
        autoFill = TRUE
    )
)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

``` r
mtcars %>% datatable2(
    bundles = dt_bundle_autofill(columns = c(1, 2, 3))
)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
mtcars %>% datatable2(
    bundles = dt_bundle_autofill(columns = c(1, 2, 3), focus = "click")
)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
mtcars %>% datatable2(
    bundles = dt_bundle_autofill(
        columns = c("mpg", "disp"), 
        focus = "click",
        .data = mtcars
    )
)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
mtcars %>% datatable2(
    bundles = list(
        dt_bundle_autofill(
            .options = list(columns = c(1, 2, 3), focus = "click")
        )
    )
)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

For comparison when calling `DT::datatable()`

``` r
mtcars %>% DT::datatable(
    extensions = "AutoFill", 
    options = list(
        autoFill = list(columns = c(1, 2, 3), focus = "click")
    )
)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

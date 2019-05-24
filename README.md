
<!-- README.md is generated from README.Rmd. Please edit that file -->

# grainchanger <img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/laurajanegraham/grainchanger.svg?branch=master)](https://travis-ci.org/laurajanegraham/grainchanger)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/laurajanegraham/grainchanger?branch=master&svg=true)](https://ci.appveyor.com/project/laurajanegraham/grainchanger)
[![Codecov test
coverage](https://codecov.io/gh/laurajanegraham/grainchanger/branch/master/graph/badge.svg)](https://codecov.io/gh/laurajanegraham/grainchanger?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/grainchanger)](https://cran.r-project.org/package=grainchanger)
[![](https://cranlogs.r-pkg.org/badges/grainchanger)](https://cran.r-project.org/package=grainchanger)
[![Project Status: Active â€“ The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

The `grainchanger` package provides functionality for data aggregation
to a coarser resolution via moving-window or direct methods.

As landscape ecologists and macroecologists, we often need to aggregate
data in order to harmonise datasets. In doing so, we often lose a lot of
information about the spatial structure and environmental heterogeneity
of data measured at finer resolution. A specific case we are considering
is where the response data (e.g. species’ atlas data) are available at a
coarser resolution to the predictor data (e.g. land-use data). We
developed this method and R package in order to overcome some of these
issues.

For more information on the background to and motivation for the
development of this method, see [Graham *et al.* 2019 in *Methods in
Ecology and Evolution*](https://doi.org/10.1111/2041-210X.13177).

### Moving-window data aggregation

The moving-window data aggregation (MWDA) method smooths an input raster
using a specified function within a moving window of a specified size
and shape prior to aggregation. This acts as a convenient wrapper for
the `focalWeight()` and `focal()` functions in the `raster` package.
Additionally, we have aimed to write efficient functions for some
oft-used metrics within landscape ecology for use within the moving
window.

![Schematic of the moving-window data aggregation
approach](man/figures/mwda_schematic.png)

The above is a graphical representation of the MWDA method. In
calculating the MWDA measure, three aspects of scale are considered.
Predictor grain is the characteristic spatial scale of a predictor
variable, that is, the resolution of the environmental data;
scale‐of‐effect determines the appropriate scale of the relationship
between predictor and response, for example, an ecological
neighbourhood; response grain is the grain of the unit into which you
are predicting, that is, the resolution of the response variable
(represented by the black lines). Note that the colour scale is
unitless. Yellow cells represent ‘high’ values and dark blue cells ‘low’
values. Panel 1 shows a close up of one of the response grain cells in
panel 2, whereas panel 2 shows all response grain cells for the study
region. Panel 3 shows the study region after aggregation.

### Direct data aggregation

The direct method simply aggregates to the grid using the specified
function, essentially acting as a wrapper for the `raster` aggregate
function.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("laurajanegraham/grainchanger")
```

## Example

### Moving-window data aggregation

The below example shows the moving-window data aggregation in action. It
aggregates a categorical raster to a grid using Shannon evenness as the
function calculated within a square moving window of 5 units. This value
is included as a column on the grid `sf` object.

``` r
library(grainchanger)
library(ggplot2)
#> Registered S3 methods overwritten by 'ggplot2':
#>   method         from 
#>   [.quosures     rlang
#>   c.quosures     rlang
#>   print.quosures rlang
library(landscapetools)

# categorical landscape
show_landscape(cat_ls, discrete = TRUE)
```

<img src="man/figures/README-mwda_example-1.png" width="100%" />

``` r

# moving-window aggregation using Shannon evenness
g_sf$mwda <- winmove_agg(coarse_dat = g_sf,
                         fine_dat = cat_ls, 
                         d = 5,
                         type = "rectangle",
                         win_fun = shei,
                         agg_fun = mean,
                         lc_class = 0:3,
                         quiet = TRUE)

ggplot(g_sf) + geom_sf(aes(fill = mwda))
```

<img src="man/figures/README-mwda_example-2.png" width="100%" />

### Direct data aggregation

The below example shows the direct data aggregation in action. It
aggregates a continuous raster to a grid using the range as the function
calculated for each cell of the larger grid. This value is included as a
column on the grid `sf` object. `var_range` is an inbuilt function in
the `grainchanger` package.

``` r
# continuous landscape
show_landscape(cont_ls)
```

<img src="man/figures/README-dda_example-1.png" width="100%" />

``` r

# direct aggregation using range
g_sf$dda <- nomove_agg(coarse_dat = g_sf,
                       fine_dat = cont_ls, 
                       agg_fun = var_range)
#> aggregation assumes all cells are rectangular
#> <U+25CF> set `is_grid = FALSE` if coarse_dat is not a grid

ggplot(g_sf) + geom_sf(aes(fill = dda))
```

<img src="man/figures/README-dda_example-2.png" width="100%" />

## Functions

There are a number of inbuilt functions in the grainchanger package,
with their usage outlined below. While it is possible to use
user-defined functions within both `winmove_agg` and `nomove_agg`, we
welcome suggestions for additional functions. Please [add as an
issue](https://github.com/laurajanegraham/grainchanger/issues) - doing
it this way means we can maximise the speed of the
function.

| Function.Name | Description                               | Additional.arguments |
| :------------ | :---------------------------------------- | :------------------- |
| prop          | Calculate the proportion of a given class | lc\_class (numeric)  |
| shdi          | Calculate the Shannon diversity           |                      |
| shei          | Calculate the Shannon evenness            | lc\_class (numeric)  |
| range         | Calculate the range of values             |                      |

## Additional utilities

### Create torus

The `create_torus` function takes as input a raster and pads it by a
specified radius, creating the effect of a torus. We developed this
function in order to avoid edge effects when testing methods on
simulated rasters (such as those from
[NLMR](https://ropensci.github.io/NLMR/)).

``` r
torus <- create_torus(cat_ls, 5)

show_landscape(torus, discrete = TRUE)
```

<img src="man/figures/README-torus-1.png" width="100%" />

## Meta

  - Please [report any issues or
    bugs](https://github.com/laurajanegraham/grainchanger/issues/new/).
  - License: GPL3
  - Get citation information for `grainchanger` in R doing
    `citation(package = 'grainchanger')`
  - Please note that the `grainchanger` project is released with a
    [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing
    to this project, you agree to abide by its terms.

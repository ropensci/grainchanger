winmoveR
===

`winmoveR` acts as a wrapper for the `raster` package and allows users to upscale fine resolution rasters to a coarser grid using specified functions. This can either be done in the classic way (aggregating using the function at the larger cell level) or by using moving windows within a given radius and shape. The moving window approach allows users to capture landscape structure at the scale at which the process acts in the landscape.

Installation
------------

To install the development version of `winmoveR`, use the following R code:

``` r
# install.packages("devtools")
devtools::install_github("laurajanegraham/winmoveR")
```

Example
-------

To update


Contributor Code of Conduct
---------------------------

To update

Dependencies
------------

To update - with links to packages

`NLMR` imports many great packages that it depends on. Many thanks to the developers of these tools:

     [1] "R (>= 3.1.0)"  " checkmate"    " dismo"        " dplyr"       
     [5] " ggplot2"      " igraph"       " magrittr"     " purrr"       
     [9] " RandomFields" " raster"       " rasterVis"    " sp"          
    [13] " spatstat"     " stats"        " tibble"       " tidyr"       
    [17] " viridis"      " extrafont"

grainchanger
===

`grainchanger` acts as a wrapper for the `raster` package and allows users to upscale fine resolution rasters to a coarser grid using specified functions. This can either be done in the classic way (aggregating using the function at the larger cell level) or by using moving windows within a given radius and shape. The moving window approach allows users to capture landscape structure at the scale at which the process acts in the landscape.

Installation
------------

To install the development version of `grainchanger`, use the following R code:

``` r
# install.packages("devtools")
devtools::install_github("laurajanegraham/grainchanger")
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

`grainchanger` imports the following package dependencies [`raster`](https://cran.r-project.org/web/packages/raster/raster.pdf), [`plyr`](https://cran.r-project.org/web/packages/plyr/index.html); [`rgeos`](https://cran.r-project.org/web/packages/rgeos/index.html); [`pbapply`](https://cran.rstudio.com/web/packages/pbapply/index.html). Thanks to the developers of these packages. 


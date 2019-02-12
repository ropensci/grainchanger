# testing the functions
test_rcpp <- function(ras, d) {
    # create a neighbourhood matrix
    nb <- raster::focalWeight(ras, d, type = "rectangle")
    nb[nb > 0] <- 1
    nb[nb == 0] <- NA
    mid = ceiling(dim(nb) / 2)
    nb[mid[1], mid[2]] <- 0
    
    # convert to matrix and get lc classes
    mat <- as.matrix(ras)
    classes <- rcpp_get_unique_values(mat)
    
    # output and convert back to raster
    out1 <- rcpp_focal_stats(mat, nb, classes)
    out1b <- raster(out1)
    raster::extent(out1b) <- extent(ras)
    return(out1b)
}

test_gc <- function(ras, d) {
    mat <- as.matrix(ras)
    classes <- rcpp_get_unique_values(mat) 
    grainchanger::winmove(ras, d, type = "rectangle", fun = "shei", lc_class = classes)
}

library(raster)
library(tidyverse)
ras <- landscapemetrics::augusta_nlcd
out1 <- test_rcpp(ras, 30)
out2 <- test_gc(ras, 30)

library(microbenchmark)
library(ggplot2)

theme_set(theme_bw())

out <- microbenchmark (
    cpp_30 = test_rcpp(ras, 30),
    gc_30 = test_gc(ras, 30),
    cpp_90 = test_rcpp(ras, 90),
    gc_90 = test_gc(ras, 90),
    cpp_300 = test_rcpp(ras, 300),
    gc_300 = test_gc(ras, 300),
    times = 10
)

df <- out %>% as_tibble() %>% 
    separate(expr, into = c("method", "window_size")) %>% 
    mutate(time = microbenchmark:::convert_to_unit(time, "t"),
           window_size = window_size %>% as.numeric %>% as.factor)

ggplot(df, aes(x = window_size, y = time, fill = method)) + 
    geom_violin() + 
    coord_flip()

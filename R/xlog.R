#' Upscale the moving window value
#' 
#' Calculate xlogx of a given value for use in `diversity()` and `diversity_nbrhd()` functions
#'@param x numeric
#'@return numeric vector

xlog <- function(x) ifelse(x==0, 0, x*log(x))


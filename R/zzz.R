#' @import RNifti
#' @importFrom Rcpp evalCpp
#' @useDynLib divest
.onLoad <- function (libname, pkgname)
{
    .Call("initialise", PACKAGE="divest")
}

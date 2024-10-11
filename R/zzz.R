#' @import RNifti
#' @importFrom Rcpp evalCpp
#' @useDynLib divest, .registration = TRUE, .fixes = "C_"
NULL

#' Report package capabilities
#' 
#' This function summarises the capabilities of the package as compiled for the
#' current machine, analogously to the `base` function [capabilities()]. It
#' identifies the support available for various input and output formats.
#' 
#' @param what A character vector of components to extract, or `NULL`,
#'   the default, which indicates the full set of capabilities.
#' @return A named logical vector, indicating whether plain JPEG, JPEG-LS and
#'   JPEG2000 DICOM transfer syntaxes are supported by the current build of the
#'   package, and also whether `zlib` is available to produce compressed NIfTI
#'   output files from [convertDicom()].
#' 
#' @seealso [readDicom()]
#' @examples
#' divest.capabilities()
#' @author Jon Clayden <code@@clayden.org>
#' @export
divest.capabilities <- function (what = NULL)
{
    caps <- .Call(C_getCapabilities)
    if (!is.null(what))
        caps <- caps[charmatch(what, names(caps), 0L)]
    return (caps)
}

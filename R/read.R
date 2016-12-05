#' Read one or more DICOM directories
#' 
#' This function is an R wrapper around the DICOM-to-NIfTI conversion routines
#' provided by \code{dcm2niix}. It scans directories containing DICOM files,
#' potentially pertaining to more than one image series, reads them and merges
#' them into a list of \code{niftiImage} objects.
#' 
#' @param path A character vector of paths to scan for DICOM files. Each will
#'   examined in turn.
#' @param flipY If \code{TRUE}, the default, then images will be flipped in the
#'   Y-axis. This is usually desirable, given the difference between
#'   orientation conventions in the DICOM and NIfTI-1 formats.
#' @param verbosity Integer value between 0 and 3, controlling the amount of
#'   output generated during the conversion.
#' @return A list of \code{niftiImage} objects, which can be easily converted
#'   to R arrays or written to NIfTI-1 format, using functions from the
#'   \code{RNifti} package. If the process fails, the result will be
#'   \code{NULL}.
#' 
#' @author Jon Clayden <code@@clayden.org>
#' @export
readDicom <- function (path, flipY = TRUE, verbosity = 0L)
{
    results <- lapply(path, function(p) .Call("readDirectory",path.expand(p),flipY,verbosity,PACKAGE="divest"))
    return (do.call(c, results))
}

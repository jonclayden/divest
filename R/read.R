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
#' @examples
#' path <- system.file("extdata", "testdata", package="divest")
#' readDicom(path)
#' @author Jon Clayden <code@@clayden.org>
#' @export
readDicom <- function (path, flipY = TRUE, verbosity = 0L, interactive = base::interactive())
{
    results <- lapply(path, function(p) {
        if (interactive)
        {
            p <- path.expand(p)
            series <- .Call("readDirectory", p, flipY, 0L, TRUE, PACKAGE="divest")
            
            nSeries <- length(series)
            if (nSeries < 1)
                return (NULL)
            
            digits <- floor(log10(nSeries))
            seriesNumbers <- sprintf(paste0("%",digits,"d: "), seq_len(nSeries))
            cat(paste0("\n", seriesNumbers, unlist(series)))
            cat("\n\nType <Enter> for all series, 0 for none, or indices separated by spaces or commas")
            selection <- readline("\nSelected series: ")
            if (selection == "")
                .Call("readDirectory", p, flipY, verbosity, FALSE, PACKAGE="divest")
            else if (selection == "0")
                return (NULL)
            else
            {
                selection <- as.integer(unlist(strsplit(selection, "[, ]+", perl=TRUE)))
                files <- do.call(c, lapply(series[selection], attr, "paths"))
                files <- paste("..", substring(files,nchar(p)+1), sep=.Platform$file.sep)
                tempDirectory <- file.path(p, ".divest")
                dir.create(tempDirectory)
                success <- file.symlink(files, tempDirectory)
                result <- .Call("readDirectory", tempDirectory, flipY, verbosity, FALSE, PACKAGE="divest")
                unlink(tempDirectory, recursive=TRUE)
                return (result)
            }
        }
        else
            .Call("readDirectory", path.expand(p), flipY, verbosity, FALSE, PACKAGE="divest")
    })
    
    return (do.call(c, results))
}

.sortInfoTable <- function (table)
{
    ordering <- with(table, order(patientName,studyDate,seriesNumber,echoNumber,phase))
    return (structure(table[ordering,], descriptions=attr(table,"descriptions")[ordering], paths=attr(table,"paths")[ordering], ordering=ordering, class=c("divest","data.frame")))
}

.readPath <- function (path, flipY, crop, forceStack, verbosity, labelFormat, singleFile, scanOnly)
{
    if (verbosity < 0L)
    {
        output <- NULL
        connection <- textConnection("output", "w", local=TRUE)
        sink(connection)
        on.exit({
            sink()
            cat(paste(c(grep("WARNING|ERROR", output, ignore.case=TRUE, perl=TRUE, value=TRUE), ""), collapse="\n"))
        })
    }
    
    .Call(C_readDirectory, path, flipY, crop, forceStack, verbosity, labelFormat, singleFile, scanOnly)
}

#' Read one or more DICOM directories
#' 
#' These functions are R wrappers around the DICOM-to-NIfTI conversion routines
#' provided by \code{dcm2niix}. They scan directories containing DICOM files,
#' potentially pertaining to more than one image series, read them and/or merge
#' them into a list of \code{niftiImage} objects.
#' 
#' The \code{labelFormat} argument describes the string format used for image
#' labels. Valid codes, each escaped with a percentage sign, include \code{a}
#' for coil number, \code{c} for image comments, \code{d} for series
#' description, \code{e} for echo number, \code{f} for the source directory,
#' \code{i} for patient ID, \code{l} for the procedure step description,
#' \code{m} for manufacturer, \code{n} for patient name, \code{p} for protocol
#' name, \code{q} for scanning sequence, \code{s} for series number, \code{t}
#' for the date and time, \code{u} for acquisition number and \code{z} for
#' sequence name.
#' 
#' @param path A character vector of paths to scan for DICOM files. Each will
#'   examined in turn. The default is the current working directory.
#'   Alternatively, a data frame like the one returned by \code{scanDicom},
#'   from which file paths will be read.
#' @param flipY If \code{TRUE}, the default, then images will be flipped in the
#'   Y-axis. This is usually desirable, given the difference between
#'   orientation conventions in the DICOM and NIfTI-1 formats.
#' @param crop If \code{TRUE}, then \code{dcm2niix} will attempt to crop excess
#'   neck slices from brain images.
#' @param forceStack If \code{TRUE}, images with the same series number will
#'   always be stacked together as long as their dimensions are compatible. If
#'   \code{FALSE}, the default, images will be separated if they differ in
#'   echo, coil or exposure number, echo time, protocol name or orientation.
#' @param verbosity Integer value between -1 and 3, controlling the amount of
#'   output generated during the conversion. A negative value will suppress all
#'   output from \code{dcm2niix} except warnings and errors.
#' @param labelFormat A \code{\link{sprintf}}-style string specifying the
#'   format to use for the final image labels. See Details.
#' @param subset If \code{path} is a data frame, an expression which will be
#'   evaluated in the context of the data frame to determine which series to
#'   convert.
#' @param interactive If \code{TRUE}, the default in interactive sessions, the
#'   requested paths will first be scanned and a list of DICOM series will be
#'   presented. You may then choose which series to convert.
#' @return The \code{readDicom} function returns a list of \code{niftiImage}
#'   objects, which can be easily converted to standard R arrays or written to
#'   NIfTI-1 format using functions from the \code{RNifti} package. The
#'   \code{scanDicom} function returns a data frame containing information
#'   about each DICOM series found.
#' 
#' @examples
#' path <- system.file("extdata", "raw", package="divest")
#' scanDicom(path)
#' readDicom(path, interactive=FALSE)
#' @author Jon Clayden <code@@clayden.org>
#' @export
readDicom <- function (path = ".", flipY = TRUE, crop = FALSE, forceStack = FALSE, verbosity = 0L, labelFormat = "T%t_N%n_S%s", subset = NULL, interactive = base::interactive())
{
    readFromTempDirectory <- function (tempDirectory, files)
    {
        # Don't overwrite an existing path
        originalTempDirectory <- tempDirectory
        i <- 1
        while (file.exists(tempDirectory))
        {
            tempDirectory <- paste(originalTempDirectory, as.character(i), sep="_")
            i <- i + 1
        }
        
        dir.create(tempDirectory)
        on.exit(unlink(tempDirectory, recursive=TRUE))
        
        success <- file.symlink(files, tempDirectory)
        if (!all(success))
        {
            unlink(tempDirectory, recursive=TRUE)
            dir.create(tempDirectory)
            success <- file.copy(files, tempDirectory)
        }
        if (!all(success))
            stop("Cannot symlink or copy files into temporary directory")
        
        .readPath(tempDirectory, flipY, crop, forceStack, verbosity, labelFormat, FALSE, FALSE)
    }
    
    usingTempDirectory <- FALSE
    if (is.data.frame(path))
    {
        subset <- eval(substitute(subset), path)
        if (!is.null(subset))
            path <- attr(path,"paths")[subset]
        else
            path <- attr(path,"paths")
        usingTempDirectory <- TRUE
    }
    
    results <- lapply(path, function(p) {
        if (usingTempDirectory)
        {
            absolute <- grepl(paste0("^([A-Za-z]:)?",.Platform$file.sep), p)
            p[!absolute] <- file.path("..", p[!absolute])
            readFromTempDirectory(".divest", p)
        }
        else if (!file.exists(p))
            warning(paste0("Path \"", p, "\" does not exist"))
        else if (!file.info(p)$isdir)
            .readPath(path.expand(p), flipY, crop, forceStack, verbosity, labelFormat, TRUE, FALSE)
        else if (interactive)
        {
            p <- path.expand(p)
            info <- .sortInfoTable(.readPath(p, flipY, crop, forceStack, min(0L,verbosity), labelFormat, FALSE, TRUE))
            
            nSeries <- nrow(info)
            if (nSeries < 1)
                return (NULL)
            
            digits <- floor(log10(nSeries)) + 1
            seriesNumbers <- sprintf(paste0("%",digits,"d: "), seq_len(nSeries))
            cat(paste0("\n", seriesNumbers, attr(info,"descriptions")))
            cat("\n\nType <Enter> for all series, 0 for none, or indices separated by spaces or commas")
            selection <- readline("\nSelected series: ")
            if (selection == "")
            {
                allResults <- .readPath(p, flipY, crop, forceStack, verbosity, labelFormat, FALSE, FALSE)
                return (allResults[attr(info,"ordering")])
            }
            else if (selection == "0")
                return (list())
            else
            {
                selection <- as.integer(unlist(strsplit(selection, "[, ]+", perl=TRUE)))
                selectedResults <- lapply(selection, function(s) {
                    files <- attr(info,"paths")[[s]]
                    files <- paste("..", substring(files,nchar(p)+1), sep=.Platform$file.sep)
                    readFromTempDirectory(file.path(p,paste0(".divest",as.character(s))), files)
                })
                return (do.call(c,selectedResults))
            }
        }
        else
            .readPath(path.expand(p), flipY, crop, forceStack, verbosity, labelFormat, FALSE, FALSE)
    })
    
    return (do.call(c, results))
}

#' @rdname readDicom
#' @export
scanDicom <- function (path = ".", forceStack = FALSE, verbosity = 0L)
{
    results <- lapply(path, function(p) .readPath(path.expand(p), TRUE, FALSE, forceStack, verbosity, "", FALSE, TRUE))
    .sortInfoTable(do.call(rbind, results))
}

.sortInfoTable <- function (table)
{
    ordering <- with(table, order(patientName,studyDate,seriesNumber,echoNumber,phase))
    return (structure(table[ordering,], descriptions=attr(table,"descriptions")[ordering], paths=attr(table,"paths")[ordering], ordering=ordering, class=c("divest","data.frame")))
}

.readPath <- function (path, flipY, crop, forceStack, verbosity, labelFormat, singleFile, depth, task = c("read","scan","sort"), outputDir = NULL)
{
    task <- match.arg(task)
    if (verbosity < 0L)
    {
        output <- NULL
        connection <- textConnection("output", "w", local=TRUE)
        sink(connection)
        on.exit({
            sink()
            cat(paste(c(grep(ifelse(verbosity < -1L, "ERROR", "WARNING|ERROR"), output, ignore.case=TRUE, perl=TRUE, value=TRUE), ""), collapse="\n"))
        })
    }
    
    .Call(C_readDirectory, path, flipY, crop, forceStack, verbosity, labelFormat, singleFile, depth, task, outputDir)
}

# Wrapper function to allow mocking in tests
.readline <- function (...) base::readline(...)

#' Read one or more DICOM directories
#' 
#' These functions are R wrappers around the DICOM-to-NIfTI conversion routines
#' provided by \code{dcm2niix}. They scan directories containing DICOM files,
#' potentially pertaining to more than one image series, read them and/or merge
#' them into a list of \code{niftiImage} objects.
#' 
#' The \code{scanDicom} function parses directories full of DICOM files and
#' returns information about the acquisition series they contain.
#' \code{readDicom} reads these files and converts them to (internal) NIfTI
#' images (whose pixel data can be extracted using \code{as.array}).
#' \code{sortDicom} renames the files, but does not convert them.
#' 
#' The \code{labelFormat} argument describes the string format used for image
#' labels and sorted files. Valid codes, each escaped with a percentage sign,
#' include \code{a} for coil number, \code{b} for the source file base name,
#' \code{c} for image comments, \code{d} for series description, \code{e} for
#' echo number, \code{f} for the source directory, \code{i} for patient ID,
#' \code{j} for the series instance UID, \code{k} for the study instance UID,
#' \code{l} for the procedure step description, \code{m} for manufacturer,
#' \code{n} for patient name, \code{p} for protocol name, \code{q} for
#' scanning sequence, \code{r} for instance number, \code{s} for series number,
#' \code{t} for the date and time, \code{u} for acquisition number, \code{v}
#' for vendor, \code{x} for study ID and \code{z} for sequence name. For
#' \code{sortDicom} the label forms the new file path, and may include one or
#' more slashes to create subdirectories. A ".dcm" suffix will be added to file
#' names if no extension is specified.
#' 
#' @param path A character vector of paths to scan for DICOM files. Each will
#'   examined in turn. The default is the current working directory.
#'   \code{readDicom} (only) will accept paths to individual DICOM files,
#'   rather than directories. Alternatively, for \code{readDicom} and
#'   \code{sortDicom}, a data frame like the one returned by \code{scanDicom},
#'   from which file paths will be read.
#' @param subset If \code{path} is a data frame, an expression which will be
#'   evaluated in the context of the data frame to determine which series to
#'   convert. Should evaluate to a logical vector. If \code{path} is a
#'   character vector, \code{scanDicom} is called on the path(s) first to
#'   produce the data frame. If this is specified, and does not evaluate to
#'   \code{NULL}, the read will be noninteractive, irrespective of the value of
#'   the \code{interactive} argument.
#' @param flipY If \code{TRUE}, the default, then images will be flipped in the
#'   Y-axis. This is usually desirable, given the difference between
#'   orientation conventions in the DICOM and NIfTI-1 formats.
#' @param crop If \code{TRUE}, then \code{dcm2niix} will attempt to crop excess
#'   neck slices from brain images.
#' @param forceStack If \code{TRUE}, images with the same series number will
#'   always be stacked together as long as their dimensions are compatible. If
#'   \code{FALSE}, the default, images will be separated if they differ in
#'   echo, coil or exposure number, echo time, protocol name or orientation.
#' @param verbosity Integer value between -2 and 3, controlling the amount of
#'   output generated during the conversion. A value of -1 will suppress all
#'   output from \code{dcm2niix} except warnings and errors; -2 also suppresses
#'   warnings.
#' @param labelFormat A \code{\link{sprintf}}-style string specifying the
#'   format to use for the final image labels or paths. See Details.
#' @param depth The maximum subdirectory depth in which to search for DICOM
#'   files, relative to each \code{path}.
#' @param interactive If \code{TRUE}, the default in interactive sessions, the
#'   requested paths will first be scanned and a list of DICOM series will be
#'   presented. You may then choose which series to convert.
#' @param nested For \code{sortDicom}, should the sorted files be created
#'   within the source directory (\code{TRUE}, the default), or in the current
#'   working directory (\code{FALSE})?
#' @param keepUnsorted For \code{sortDicom}, should the unsorted files be left
#'   in place, or removed after they are copied into their new locations? The
#'   default, \code{FALSE}, corresponds to a move rather than a copy. If
#'   creating new files fails then the old ones will not be deleted.
#' @return The \code{readDicom} function returns a list of \code{niftiImage}
#'   objects, which can be easily converted to standard R arrays or written to
#'   NIfTI-1 format using functions from the \code{RNifti} package. The
#'   \code{scanDicom} function returns a data frame containing information
#'   about each DICOM series found. \code{sortDicom} is mostly called for its
#'   side-effect, but also (invisibly) returns a list detailing source and
#'   target paths.
#' 
#' @examples
#' path <- system.file("extdata", "raw", package="divest")
#' scanDicom(path)
#' readDicom(path, interactive=FALSE)
#' @author Jon Clayden <code@@clayden.org>
#' @export
readDicom <- function (path = ".", subset = NULL, flipY = TRUE, crop = FALSE, forceStack = FALSE, verbosity = 0L, labelFormat = "T%t_N%n_S%s", depth = 5L, interactive = base::interactive())
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
        
        dir.create(tempDirectory, recursive=TRUE)
        on.exit(unlink(tempDirectory, recursive=TRUE))
        
        success <- file.symlink(files, tempDirectory)
        if (!all(success))
        {
            unlink(tempDirectory, recursive=TRUE)
            dir.create(tempDirectory, recursive=TRUE)
            success <- file.copy(files, tempDirectory)
        }
        if (!all(success))
            stop("Cannot symlink or copy files into temporary directory")
        
        .readPath(tempDirectory, flipY, crop, forceStack, verbosity, labelFormat, FALSE, depth, "read")
    }
    
    usingTempDirectory <- FALSE
    if (is.data.frame(path))
    {
        subset <- eval(substitute(subset), path)
        if (!is.null(subset))
            path <- attr(path,"paths")[as.logical(subset)]
        else
            path <- attr(path,"paths")
        usingTempDirectory <- TRUE
    }
    else if (!missing(subset))
    {
        info <- scanDicom(path, forceStack, verbosity, labelFormat)
        subset <- eval(substitute(subset), info)
        if (!is.null(subset))
        {
            path <- attr(info,"paths")[as.logical(subset)]
            usingTempDirectory <- TRUE
        }
    }
    
    results <- lapply(path, function(p) {
        if (usingTempDirectory)
        {
            absolute <- grepl(paste0("^([A-Za-z]:)?",.Platform$file.sep), p)
            p[!absolute] <- file.path(getwd(), p[!absolute])
            readFromTempDirectory(file.path(tempdir(),"divest"), p)
        }
        else if (!file.exists(p))
        {
            warning(paste0("Path \"", p, "\" does not exist"))
            return (NULL)
        }
        else if (!file.info(p)$isdir)
            .readPath(path.expand(p), flipY, crop, forceStack, verbosity, labelFormat, TRUE, depth, "read")
        else if (interactive)
        {
            p <- path.expand(p)
            info <- .sortInfoTable(.readPath(p, flipY, crop, forceStack, min(0L,verbosity), labelFormat, FALSE, depth, "scan"))
            
            nSeries <- nrow(info)
            if (nSeries < 1)
                return (NULL)
            
            digits <- floor(log10(nSeries)) + 1
            seriesNumbers <- sprintf(paste0("%",digits,"d: "), seq_len(nSeries))
            cat(paste0("\n", seriesNumbers, attr(info,"descriptions")))
            cat("\n\nType <Enter> for all series, 0 for none, or indices separated by spaces or commas")
            selection <- .readline("\nSelected series: ")
            if (selection == "")
            {
                allResults <- .readPath(p, flipY, crop, forceStack, verbosity, labelFormat, FALSE, depth, "read")
                return (allResults[attr(info,"ordering")])
            }
            else if (selection == "0")
                return (list())
            else
            {
                selection <- as.integer(unlist(strsplit(selection, "[, ]+", perl=TRUE)))
                selectedResults <- lapply(selection, function(s) {
                    files <- attr(info,"paths")[[s]]
                    absolute <- grepl(paste0("^([A-Za-z]:)?",.Platform$file.sep), files)
                    files[!absolute] <- file.path(getwd(), files[!absolute])
                    readFromTempDirectory(file.path(tempdir(),paste0("divest",as.character(s))), files)
                })
                return (do.call(c,selectedResults))
            }
        }
        else
            .readPath(path.expand(p), flipY, crop, forceStack, verbosity, labelFormat, FALSE, depth, "read")
    })
    
    return (do.call(c, results))
}

#' @rdname readDicom
#' @export
sortDicom <- function (path = ".", forceStack = FALSE, verbosity = 0L, labelFormat = "T%t_N%n_S%s/%b", depth = 5L, nested = TRUE, keepUnsorted = FALSE)
{
    if (nested)
        info <- .readPath(path, FALSE, FALSE, forceStack, verbosity, labelFormat, FALSE, depth, "sort")
    else
        info <- .readPath(path, FALSE, FALSE, forceStack, verbosity, labelFormat, FALSE, depth, "sort", ".")
    
    if (!keepUnsorted && length(info$source) == length(info$target))
    {
        inPlace <- (normalizePath(info$source) == normalizePath(info$target))
        unlink(info$source[!inPlace])
    }
    
    invisible(info)
}

#' @rdname readDicom
#' @export
scanDicom <- function (path = ".", forceStack = FALSE, verbosity = 0L, labelFormat = "T%t_N%n_S%s", depth = 5L)
{
    results <- lapply(path, function(p) {
        if (file.info(p)$isdir)
            .readPath(path.expand(p), TRUE, FALSE, forceStack, verbosity, labelFormat, FALSE, depth, "scan")
        else
            warning(paste0("Path \"", p, "\" does not point to a directory"))
    })
    
    .sortInfoTable(Reduce(function(x,y) structure(rbind(x,y), descriptions=c(attr(x,"descriptions"),attr(y,"descriptions")), paths=c(attr(x,"paths"),attr(y,"paths"))), results))
}

.tempDirectory <- function ()
{
    # Don't overwrite an existing temporary directory
    originalTempDirectory <- tempDirectory <- file.path(tempdir(), paste("divest",Sys.getpid(),sep="_"))
    suffix <- 1
    while (file.exists(tempDirectory))
    {
        tempDirectory <- paste(originalTempDirectory, as.character(suffix), sep="_")
        suffix <- suffix + 1
    }
    
    dir.create(tempDirectory, recursive=TRUE)
    return (tempDirectory)
}

.resolvePaths <- function (path, subset = NULL, dirsOnly = FALSE)
{
    # Data frame case (caller should handle subsets with character path)
    if (is.data.frame(path))
    {
        if (!is.null(subset))
            paths <- unlist(attr(path,"paths")[subset])
        else
            paths <- unlist(attr(path,"paths"))
        
        tempDirectory <- .tempDirectory()
        success <- file.symlink(paths, tempDirectory)
        if (!all(success))
        {
            unlink(tempDirectory, recursive=TRUE)
            dir.create(tempDirectory, recursive=TRUE)
            success <- file.copy(paths, tempDirectory)
        }
        
        if (all(success))
            return (structure(tempDirectory, temporary=TRUE))
        else
            stop("Cannot symlink or copy files into temporary directory")
    }
    
    # Path should now only be a character vector
    for (i in seq_along(path))
    {
        if (!file.exists(path[i]))
        {
            warning(paste0("Path \"", path[i], "\" does not exist"))
            path <- path[-i]
        }
        else if (dirsOnly && !file.info(path[i])$isdir)
        {
            warning(paste0("Path \"", path[i], "\" does not point to a directory"))
            path <- path[-i]
        }
        else
            path[i] <- path.expand(path[i])
    }
    return (path)
}

.sortInfoTable <- function (table)
{
    ordering <- with(table, order(patientName,studyDate,seriesNumber,echoNumber,phase))
    return (structure(table[ordering,], descriptions=attr(table,"descriptions")[ordering], paths=attr(table,"paths")[ordering], ordering=ordering, class=c("divest","data.frame")))
}

.readPath <- function (path, flipY, crop, forceStack, verbosity, labelFormat, singleFile, depth, task = c("read","convert","scan","sort"), outputDir = NULL)
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
    
    if (is.null(outputDir))
        outputDir <- .tempDirectory()
    results <- .Call(C_readDirectory, path, flipY, crop, forceStack, verbosity, labelFormat, singleFile, depth, task, outputDir)
    
    if (task == "read")
    {
        convertAttributes <- !isTRUE(getOption("divest.bidsAttributes"))
        addAttributes <- function (im)
        {
            attribs <- attributes(im)
            if (!is.null(attr(im, ".bidsJson")))
            {
                attribs <- c(attribs, jsonlite::fromJSON(attr(im,".bidsJson"),simplifyVector=TRUE))
                attr(im, ".bidsJson") <- NULL
            }
            else
            {
                jsonPath <- file.path(outputDir, paste(as.character(im),"json",sep="."))
                if (file.exists(jsonPath))
                    attribs <- c(attribs, jsonlite::read_json(jsonPath,simplifyVector=TRUE))
            }
            
            if (convertAttributes)
                attribs <- bidsToDivest(attribs)
            
            attributes(im) <- attribs
            return (im)
        }
        results <- lapply(results, addAttributes)
    }
    
    return (results)
}

# Wrapper function to allow mocking in tests
.readline <- function (...) base::readline(...)

# Similar to utils::menu(), but defaults to selecting everything, and allows
# comma separation and ranges (using colons or hyphens)
# Currently lacks column formatting, since strings are generally quite long
.menu <- function (choices)
{
    choices <- as.character(choices)
    nChoices <- length(choices)
    
    if (nChoices < 1L)
        return (integer(0))
    
    digits <- as.integer(floor(log10(nChoices)) + 1)
    numbers <- sprintf(paste0("%",digits,"d: "), seq_len(nChoices))
    
    cat(paste0("\n", numbers, choices))
    cat("\n\nType <Enter> to select everything, 0 for nothing, or indices separated by spaces or commas")
    selection <- .readline("\nSelection: ")
    
    if (selection == "")
        selection <- seq_len(nChoices)
    else if (selection == "0")
        selection <- integer(0)
    else
    {
        # Split into elements separated by commas or spaces, and resolve ranges
        parts <- unlist(strsplit(selection, "[, ]+", perl=TRUE))
        selection <- as.integer(unlist(lapply(parts, function (str) {
            str <- sub("(\\d)-(\\d)", "\\1:\\2", str, perl=TRUE)
            eval(parse(text=str))
        })))
    }
    
    return (selection)
}

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
#' @export readDicom convertDicom
readDicom <- convertDicom <- function (path = ".", subset = NULL, flipY = TRUE, crop = FALSE, forceStack = FALSE, verbosity = 0L, labelFormat = "T%t_N%n_S%s", depth = 5L, interactive = base::interactive(), output = NULL)
{
    if (!is.data.frame(path) && !missing(subset))
        path <- scanDicom(path, forceStack, verbosity, labelFormat)
    if (is.data.frame(path))
        subset <- eval(substitute(subset), path)
    
    path <- .resolvePaths(path, subset)
    task <- ifelse(is.null(output), "read", "convert")
    
    if (any(attr(path, "temporary")))
        on.exit(unlink(path[attr(path,"temporary")], recursive=TRUE))
    
    results <- lapply(path, function(p) {
        if (!file.info(p)$isdir)
            .readPath(p, flipY, crop, forceStack, verbosity, labelFormat, TRUE, depth, task, output)
        else if (interactive && is.null(subset))
        {
            info <- .sortInfoTable(.readPath(p, flipY, crop, forceStack, min(0L,verbosity), labelFormat, FALSE, depth, "scan"))
            
            selection <- .menu(attr(info,"descriptions"))
            if (identical(selection, seq_len(nrow(info))))
            {
                allResults <- .readPath(p, flipY, crop, forceStack, verbosity, labelFormat, FALSE, depth, task, output)
                return (allResults[attr(info,"ordering")])
            }
            else
            {
                selectedResults <- lapply(.resolvePaths(info,selection), .readPath, flipY, crop, forceStack, verbosity, labelFormat, FALSE, depth, task, output)
                return (do.call(c, selectedResults))
            }
        }
        else
            .readPath(p, flipY, crop, forceStack, verbosity, labelFormat, FALSE, depth, task, output)
    })
    
    return (do.call(c, results))
}

#' @rdname readDicom
#' @export
sortDicom <- function (path = ".", forceStack = FALSE, verbosity = 0L, labelFormat = "T%t_N%n_S%s/%b", depth = 5L, nested = TRUE, keepUnsorted = FALSE)
{
    if (nested)
        info <- .readPath(path, FALSE, FALSE, forceStack, verbosity, labelFormat, FALSE, depth, "sort", path)
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

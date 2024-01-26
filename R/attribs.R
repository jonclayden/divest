.RNiftiAttribs <- "^\\.|^(image|pix)dim$|^pixunits$|^class$"

.Bids <- list(
    mappingFromJson=c(MagneticFieldStrength="fieldStrength",
                      ManufacturersModelName="scannerModelName",
                      SpacingBetweenSlices="sliceSpacing",
                      TotalReadoutTime="effectiveReadoutTime",
                      MultibandAccelerationFactor="multibandFactor",
                      ImageComments="comments"),
    mappingToJson=c(fieldStrength="MagneticFieldStrength",
                    scannerModelName="ManufacturersModelName",
                    sliceSpacing="SpacingBetweenSlices",
                    effectiveReadoutTime="TotalReadoutTime",
                    multibandFactor="MultibandAccelerationFactor",
                    comments="ImageComments"),
    toScale="^(Echo|Repetition|Inversion)Time$")

#' Extended image attributes
#' 
#' These functions extract and manipulate medical image attributes that go
#' beyond the core metadata associated with the NIfTI-1 file format. The
#' \code{imageAttributes} function
#' 
#' The DICOM format can encapsulate copious amounts of metadata about the scan
#' and the patient, which can be useful for more advanced or research-focussed
#' post-processing methods. This information is extracted during the DICOM to
#' NIfTI conversion process
#' 
#' @param x An R object. For \code{imageAttributes} this would usually be an
#'   image object, like those returned by \code{\link{readDicom}}.
#'   \code{bidsToDivest} is S3 generic, with methods for lists (of named
#'   attributes) and character strings (giving the path to a BIDS JSON file),
#'   as well as a default method that handles image objects.
#' @return A list of image attributes, possibly with its naming convention
#'   changed relative to the input.
#' 
#' @note Attributes may include sensitive or identifiable information such as
#'   a patient's name, sex, date of birth, etc., if this was included in the
#'   original DICOM files. These functions make no attempt to anonymise this
#'   metadata, and so this must be handled by the user if necessary.
#' 
#' @examples
#' path <- system.file("extdata", "raw", package="divest")
#' scanDicom(path)
#' readDicom(path, interactive=FALSE)
#' @author Jon Clayden <code@@clayden.org>
#' @export
imageAttributes <- function (x)
{
    attribs <- attributes(x)
    if (length(attribs) == 0L || is.null(names(attribs)))
        return (NULL)
    attribs <- attribs[!grepl(.RNiftiAttribs,names(attribs),perl=TRUE) & names(attribs) != ""]
    if (length(attribs) == 0L)
        return (NULL)
    else
        return (attribs)
}

#' @rdname imageAttributes
#' @export
bidsToDivest <- function (x)
{
    UseMethod("bidsToDivest")
}

#' @rdname imageAttributes
#' @export
bidsToDivest.list <- function (x)
{
    bids <- x
    divest <- list()
    
    passthrough <- grepl(.RNiftiAttribs, names(bids), perl=TRUE)
    if (any(passthrough))
        divest <- bids[passthrough]
    
    for (bidsName in names(bids)[!passthrough])
    {
        value <- bids[[bidsName]]
        if (is.character(value) && all(grepl("^\\s*$", value, perl=TRUE)))
            next
        
        if (bidsName == "PhaseEncodingDirection" && grepl("^([ijk])(-)?$", value, perl=TRUE))
        {
            divest$phaseEncodingDirection <- substring(value,1,1)
            divest$phaseEncodingSign <- ifelse(nchar(value)==1L, 1L, -1L)
            next
        }
        
        # Attribute names mostly reflect the BIDS ones, but with the first letter downcased
        # A few exceptions are mapped explicitly
        if (bidsName %in% names(.Bids$mappingFromJson))
            name <- .Bids$mappingFromJson[[bidsName]]
        else if (grepl("^[A-Z][a-z]", bidsName, perl=TRUE))
            name <- paste0(tolower(substring(bidsName,1,1)), substring(bidsName,2))
        else
            name <- bidsName
        
        # BIDS always uses seconds for time fields, but we use milliseconds in some places
        if (grepl(.Bids$toScale, bidsName, perl=TRUE))
            value <- value * 1e3
        divest[[name]] <- value
    }
    
    return (divest)
}

#' @rdname imageAttributes
#' @export
bidsToDivest.character <- function (x)
{
    return (bidsToDivest(jsonlite::fromJSON(x, simplifyVector=TRUE)))
}

#' @rdname imageAttributes
#' @export
bidsToDivest.default <- function (x)
{
    return (bidsToDivest.list(imageAttributes(x)))
}

#' @rdname imageAttributes
#' @export
divestToBids <- function (x)
{
    bids <- list()
    
    if (all(c("phaseEncodingDirection","phaseEncodingSign") %in% names(x)))
    {
        bids$PhaseEncodingDirection <- paste0(x$phaseEncodingDirection, ifelse(x$phaseEncodingSign < 0,"-",""))
        x <- x[setdiff(names(x), c("phaseEncodingDirection","phaseEncodingSign"))]
    }
    
    for (name in names(x))
    {
        value <- x[[name]]
        
        if (name %in% names(.Bids$mappingToJson))
            bidsName <- .Bids$mappingToJson[[name]]
        else if (grepl("^[a-z]", bidsName, perl=TRUE))
            bidsName <- paste0(toupper(substring(name,1,1)), substring(name,2))
        else
            bidsName <- name
        
        if (grepl(.Bids$toScale, bidsName, perl=TRUE))
            value <- value / 1e3
        bids[[bidsName]] <- value
    }
    
    return (jsonlite::toJSON(bids, auto_unbox=TRUE, digits=NA, pretty=TRUE))
}

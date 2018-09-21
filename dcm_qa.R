library(divest)
library(RNifti)
library(jsonlite)

options(warn=1)

# dcm_qa.R: A script to test divest against the dcm_qa battery
# 
# The dcm_qa dataset <https://github.com/neurolabusc/dcm_qa> consists of
# various DICOM file-sets acquired in different ways. It is used to validate
# DICOM-to-NIfTI conversion. Its reference outputs are in NIfTI and BIDS JSON
# formats.
# 
# This script converts the dataset using divest's readDicom() function. For
# each test case it then checks NIfTI metadata against the equivalent reference
# file, and compares image attributes against equivalents from the relevant
# BIDS JSON file. Differences in the names or units of certain fields are
# handled appropriately.
# 
# Several fields are currently captured in BIDS metadata but not in divest's
# image attributes. These are reported by the script for information, but their
# absence is not an error.
# 
# Usage: Rscript dcm_qa.R
# The divest, RNifti and jsonlite packages must be installed. The dcm_qa
# dataset should be present in the working directory.
# Rscript's return value can be used to check for success or failure.

root <- commandArgs(TRUE)
if (length(root) != 1)
    stop("A single argument should be specified, giving the root directory of the test battery")

images <- readDicom(file.path(root,"In"), interactive=FALSE, labelFormat="%p_%s", verbosity=-1)
labels <- unlist(images)

ignoreFields <- c("ImageType", "PhaseEncodingDirection")
scaleFields <- c("EchoTime", "RepetitionTime")
nameMapping <- c(MagneticFieldStrength="fieldStrength", ManufacturersModelName="scannerModelName", TotalReadoutTime="effectiveReadoutTime")
missingFields <- NULL

for (i in seq_along(images))
{
    refStem <- file.path(root, "Ref", labels[i])
    refImageFile <- paste(refStem, "nii", sep=".")
    refMetadataFile <- paste(refStem, "json", sep=".")
    
    if (!file.exists(refImageFile))
        warning("Reference image file ", refImageFile, " not present")
    else
    {
        refImage <- readNifti(refImageFile, internal=TRUE)
        if (!isTRUE(all.equal(dumpNifti(refImage), dumpNifti(images[[i]]))))
            stop("NIfTI metadata in image file ", refImageFile, " does not match")
    }
    
    if (!file.exists(refMetadataFile))
        warning("Reference metadata file ", refMetadataFile, " not present")
    else
    {
        metadata <- attributes(images[[i]])
        refMetadata <- read_json(refMetadataFile, simplifyVector=TRUE)
        for (bidsFieldName in names(refMetadata))
        {
            # Some fields are represented differently in the two cases. For now
            # they are skipped
            if (bidsFieldName %in% ignoreFields)
                next
            
            # Field names in divest mostly reflect the BIDS ones, but with the
            # first letter downcased. A few exceptions are mapped explicitly
            if (bidsFieldName %in% names(nameMapping))
                divestFieldName <- nameMapping[[bidsFieldName]]
            else
                divestFieldName <- paste0(tolower(substring(bidsFieldName,1,1)), substring(bidsFieldName,2))
            
            if (divestFieldName %in% names(metadata))
            {
                # BIDS always uses seconds for time fields, but divest uses
                # milliseconds in some places
                if (bidsFieldName %in% scaleFields)
                    metadata[[divestFieldName]] <- metadata[[divestFieldName]] / 1e3
                
                if (length(metadata[[divestFieldName]]) != length(refMetadata[[bidsFieldName]]))
                    stop("Length mismatch for field ", bidsFieldName)
                else if (!isTRUE(all.equal(metadata[[divestFieldName]], refMetadata[[bidsFieldName]], tolerance=1e-4)))
                    stop("BIDS field ", bidsFieldName, " does not match: ", metadata[[divestFieldName]], " vs. ", refMetadata[[bidsFieldName]])
            }
            else
                missingFields <- union(missingFields, bidsFieldName)
        }
    }
}

if (length(missingFields) > 0)
    cat(paste0("Fields not captured: ", paste(missingFields,collapse=", "), "\n\n"))

cat("All tests passed!  \\o/\n")

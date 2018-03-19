library(divest)
library(RNifti)
library(jsonlite)

options(warn=1)

images <- readDicom(file.path("dcm_qa","In"), interactive=FALSE, labelFormat="%p_%s", verbosity=-1)
labels <- unlist(images)

ignoreFields <- c("ImageType", "PhaseEncodingDirection")
scaleFields <- c("EchoTime", "RepetitionTime")
nameMapping <- c(MagneticFieldStrength="fieldStrength", ManufacturersModelName="scannerModelName", TotalReadoutTime="effectiveReadoutTime")
missingFields <- NULL

for (i in seq_along(images))
{
    refStem <- file.path("dcm_qa", "Ref", labels[i])
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
            if (bidsFieldName %in% ignoreFields)
                next
            
            if (bidsFieldName %in% names(nameMapping))
                divestFieldName <- nameMapping[[bidsFieldName]]
            else
                divestFieldName <- paste0(tolower(substring(bidsFieldName,1,1)), substring(bidsFieldName,2))
            
            if (divestFieldName %in% names(metadata))
            {
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

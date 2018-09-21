context("dcm2niix QA battery")

checkdir <- function (dir)
{
    images <- readDicom(file.path(dir,"In"), interactive=FALSE, labelFormat="%p_%s", verbosity=-1)
    labels <- unlist(images)

    ignoreFields <- c("ImageType", "PhaseEncodingDirection")
    scaleFields <- c("EchoTime", "RepetitionTime")
    nameMapping <- c(MagneticFieldStrength="fieldStrength", ManufacturersModelName="scannerModelName", TotalReadoutTime="effectiveReadoutTime")
    missingFields <- NULL

    for (i in seq_along(images))
    {
        refStem <- file.path(dir, "Ref", labels[i])
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
            refMetadata <- jsonlite::read_json(refMetadataFile, simplifyVector=TRUE)
            for (bidsFieldName in names(refMetadata))
            {
                # Some fields are represented differently in the two cases. For now they are skipped
                if (bidsFieldName %in% ignoreFields)
                    next
            
                # Field names in divest mostly reflect the BIDS ones, but with the first letter downcased
                # A few exceptions are mapped explicitly
                if (bidsFieldName %in% names(nameMapping))
                    divestFieldName <- nameMapping[[bidsFieldName]]
                else
                    divestFieldName <- paste0(tolower(substring(bidsFieldName,1,1)), substring(bidsFieldName,2))
            
                if (divestFieldName %in% names(metadata))
                {
                    # BIDS always uses seconds for time fields, but divest uses milliseconds in some places
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
}

test_that("dcm2niix QA battery matches", {
    skip_on_cran()
    skip_if_not_installed("jsonlite")
    
    expect_silent(checkdir("dcm_qa"))
    expect_silent(checkdir("dcm_qa_nih"))
})

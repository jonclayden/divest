context("Running over QA test batteries")

test_battery <- function (root)
{
    if (!file.exists(root))
        skip(paste0("QA battery \"", basename(root), "\" not available"))
    
    images <- readDicom(file.path(root,"In"), interactive=FALSE, labelFormat="%p_%s", verbosity=-2)
    labels <- unlist(images)
    
    ignoreFields <- c("ImageType", "PhaseEncodingDirection")
    scaleFields <- c("EchoTime", "RepetitionTime")
    nameMapping <- c(MagneticFieldStrength="fieldStrength", ManufacturersModelName="scannerModelName", SpacingBetweenSlices="sliceSpacing", TotalReadoutTime="effectiveReadoutTime", MultibandAccelerationFactor="multibandFactor", ImageComments="comments")
    
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
            expect_equal(niftiHeader(refImage), niftiHeader(images[[i]]))
        }
        
        if (!file.exists(refMetadataFile))
            warning("Reference metadata file ", refMetadataFile, " not present")
        else
        {
            metadata <- attributes(images[[i]])
            refMetadata <- jsonlite::read_json(refMetadataFile, simplifyVector=TRUE)
            for (bidsFieldName in names(refMetadata))
            {
                # Some fields are represented differently in the two cases - for now they are skipped
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
                    
                    expect_equal(metadata[[divestFieldName]], refMetadata[[bidsFieldName]], tolerance=1e-4)
                }
            }
        }
    }
}

test_that("main QA battery passes", {
    test_battery("dcm_qa")
})

test_that("NIH QA battery passes", {
    test_battery("dcm_qa")
})

test_that("UIH QA battery passes", {
    test_battery("dcm_qa")
})

context("Running over QA test batteries")

test_battery <- function (root, labelFormat = "%p_%s")
{
    if (!file.exists(root))
        skip(paste0("QA battery \"", basename(root), "\" not available"))
    
    images <- readDicom(file.path(root,"In"), interactive=FALSE, labelFormat=labelFormat, verbosity=-2)
    labels <- unlist(images)
    
    ignoreFields <- c("ImageType", "PhaseEncodingDirection")
    scaleFields <- c("EchoTime", "RepetitionTime", "InversionTime")
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
            refImage <- RNifti::readNifti(refImageFile, internal=TRUE)
            expect_equal(RNifti::niftiHeader(refImage), RNifti::niftiHeader(images[[i]]), info=labels[i], tolerance=1e-5)
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
                    
                    expect_equal(metadata[[!!divestFieldName]], refMetadata[[!!bidsFieldName]], info=labels[i], tolerance=1e-4)
                }
            }
        }
        
        # These files only apply to diffusion sequences
        refBvalFile <- paste(refStem, "bval", sep=".")
        refBvecFile <- paste(refStem, "bvec", sep=".")
        
        if (file.exists(refBvalFile) && file.exists(refBvecFile))
        {
            bValues <- drop(as.matrix(read.table(refBvalFile)))
            bVectors <- t(as.matrix(read.table(refBvecFile)))
            expect_equivalent(metadata$bValues, bValues, tolerance=1e-4)
            expect_equivalent(metadata$bVectors, bVectors, tolerance=1e-4)
        }
    }
}

test_that("main QA battery passes", {
    test_battery("../dcm_qa")
})

test_that("NIH QA battery passes", {
    test_battery("../dcm_qa_nih")
})

test_that("UIH QA battery passes", {
    test_battery("../dcm_qa_uih", "%p_%s_%t")
})

test_that("slice timing QA battery passes", {
    test_battery("../dcm_qa_stc", "%v_%p_%s")
})

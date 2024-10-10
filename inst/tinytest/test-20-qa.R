options(divest.bidsAttributes=TRUE)

ignoreFields <- c("PulseSequenceName", "ConversionSoftwareVersion")

test_battery <- function (root, labelFormat = "%p_%s")
{
    if (!file.exists(root))
        return (invisible(NULL))
    
    images <- readDicom(file.path(root,"In"), interactive=FALSE, labelFormat=labelFormat, verbosity=-2)
    labels <- unlist(images)
    
    refStems <- file.path(root, "Ref", labels)
    refFiles <- list(image=paste(refStems,"nii",sep="."),
                     metadata=paste(refStems,"json",sep="."),
                     bval=paste(refStems,"bval",sep="."),
                     bvec=paste(refStems,"bvec",sep="."))
    refFilesPresent <- lapply(refFiles, file.exists)
    
    # expect_setequal(list.files(file.path(root,"Ref"), "\\.nii$"), basename(refFiles$image))
    
    for (i in seq_along(images))
    {
        if (refFilesPresent$image[i])
        {
            refImage <- RNifti::readNifti(refFiles$image[i], internal=TRUE)
            expect_equal(RNifti::niftiHeader(refImage), RNifti::niftiHeader(images[[i]]), info=labels[i], tolerance=1e-5)
        }
        
        if (refFilesPresent$metadata[i])
        {
            metadata <- attributes(images[[i]])
            refMetadata <- jsonlite::read_json(refFiles$metadata[i], simplifyVector=TRUE)
            fields <- setdiff(names(refMetadata), ignoreFields)
            # Check we have all the metadata we expect
            # If not, this test will fail but we remove the names to avoid a subsequent error
            missingFields <- setdiff(fields, names(metadata))
            expect_length(missingFields, 0L, info=missingFields)
            fields <- intersect(fields, names(metadata))
            expect_equal(refMetadata[fields], metadata[fields], tolerance=1e-4)
        }
        
        # These files only apply to diffusion sequences
        if (refFilesPresent$bval[i] && refFilesPresent$bvec[i])
        {
            bValues <- drop(as.matrix(read.table(refFiles$bval[i])))
            bVectors <- t(as.matrix(read.table(refFiles$bvec[i])))
            expect_equivalent(metadata$bValues, bValues, tolerance=1e-4)
            expect_equivalent(metadata$bVectors, bVectors, tolerance=1e-4)
        }
    }
}

# Main QA test battery
test_battery("dcm_qa")

# NIH QA battery
test_battery("dcm_qa_nih")

# UIH QA battery
test_battery("dcm_qa_uih", "%p_%s_%t")

# Slice timing QA battery
test_battery("dcm_qa_stc", "%v_%p_%s")

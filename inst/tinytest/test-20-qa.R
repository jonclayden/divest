options(divest.bidsAttributes=TRUE)

ignoreFields <- c("ConversionSoftwareVersion", "BodyPartExamined", "ImageType")
ignoreImages <- character(0)

# The refLabels argument maps image labels to reference file stems, for cases
# where they differ from dcm2niix's own file names; it is called with the
# labels and the file type (e.g. "image" or "metadata")
test_battery <- function (root, labelFormat = "%p_%s", refLabels = function(labels,type) labels)
{
    if (!file.exists(root))
        return (invisible(NULL))
    
    images <- readDicom(file.path(root,"In"), interactive=FALSE, labelFormat=labelFormat, verbosity=-2)
    labels <- unlist(images)
    
    extensions <- c(image="nii", metadata="json", bval="bval", bvec="bvec")
    refFiles <- lapply(names(extensions), function(type) {
        file.path(root, "Ref", paste(refLabels(labels,type), extensions[[type]], sep="."))
    })
    names(refFiles) <- names(extensions)
    refFilesPresent <- lapply(refFiles, file.exists)
    
    # expect_setequal(list.files(file.path(root,"Ref"), "\\.nii$"), basename(refFiles$image))
    
    for (i in seq_along(images))
    {
        if (labels[i] %in% ignoreImages)
            next
        
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
            expect_equal(refMetadata[fields], metadata[fields], info=labels[i], tolerance=1e-4)
        }
        
        # These files only apply to diffusion sequences
        if (refFilesPresent$bval[i] && refFilesPresent$bvec[i])
        {
            bValues <- drop(as.matrix(read.table(refFiles$bval[i])))
            bVectors <- t(as.matrix(read.table(refFiles$bvec[i])))
            expect_equivalent(metadata$bValues, bValues, info=labels[i], tolerance=1e-4)
            expect_equivalent(metadata$bVectors, bVectors, info=labels[i], tolerance=1e-4)
        }
    }
}

# Main QA test battery
test_battery("dcm_qa")

# Slice timing QA battery
test_battery("dcm_qa_stc", "%v_%p_%s")

# CT QA battery
# dcm2niix saves tilt-corrected images as "_Tilt_1", alongside the uncorrected
# image and its sidecar, but divest saves only the corrected image, as "_Tilt"
test_battery("dcm_qa_ct", refLabels=function(labels, type) {
    sub("_Tilt$", ifelse(type == "image", "_Tilt_1", ""), labels)
})

# Enhanced DICOM batteries
test_battery("dcm_qa_enh", "%v_%p_%s")
test_battery("dcm_qa_enh_compressed", "%s_%p")

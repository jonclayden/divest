options(divest.bidsAttributes=FALSE)

# Read all
path <- system.file("extdata", "raw", package="divest")
expect_stdout(d <- readDicom(path,interactive=FALSE), "Found 4 DICOM")
expect_stdout(readDicom(path,interactive=FALSE,verbosity=-1), "WARNING")

# Check results
expect_identical(length(d), 2L)
i <- which(sapply(d,RNifti::ndim) == 3L)
expect_equal(dim(d[[i]]), c(2,224,256))
expect_equal(attr(d[[i]],"flipAngle"), 15)

# Check all attributes
# NB. String sort order is locale-dependent, so use stored names directly for indexing
attributes <- imageAttributes(d[[i]])
attrNames <- setdiff(names(attributes), "conversionSoftwareVersion")
storedAttrNames <- readRDS("attrib_names.rds")
expect_true(setequal(attrNames, storedAttrNames))
expect_equivalent_to_reference(attributes[storedAttrNames], "attributes.rds")

origin <- RNifti::worldToVoxel(c(0,0,0), d[[i]])
expect_equal(round(origin), c(-16,95,135))

# Cropping
expect_stdout(d <- readDicom(path,interactive=FALSE,crop=TRUE), "Cropping")
i <- which(sapply(d,RNifti::ndim) == 3L)
expect_equal(dim(d[[i]]), c(2,224,170))

# Subsets
expect_stdout(d <- scanDicom(path), "Found 4 DICOM")
expect_equal(d$repetitionTime, c(4100,11))
expect_stdout(readDicom(d,repetitionTime==4100), "Found 2 DICOM")
expect_stdout(readDicom(path,repetitionTime==4100,interactive=FALSE), "Found 2 DICOM")

# Single files
expect_stdout(d <- readDicom(file.path(path,"01.dcm"),interactive=FALSE), "Convert 1 DICOM")
expect_equal(dim(d[[1]]), c(224,256,1))
expect_warning(readDicom(file.path(path,"nonsense"),interactive=FALSE), "does not exist")

# Depth argument
expect_stdout(d <- readDicom(file.path(path,".."),depth=0L,interactive=FALSE), "No valid DICOM files")
expect_length(d, 0L)

if (at_home())
{
    # Monkey-patch the .readline function to simulate interactivity
    ns <- getNamespace("divest")
    unlockBinding(".readline", ns)
    assign(".readline", function(...) "1", envir=ns)
    
    # (Pseudo-)interactivity
    expect_stdout(d <- readDicom(path,interactive=TRUE), "Found 2 DICOM")
    expect_equal(unlist(d), "T0_N_S8")
}

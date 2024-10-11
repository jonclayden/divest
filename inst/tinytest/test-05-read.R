options(divest.bidsAttributes=FALSE)

caps <- divest.capabilities()
expect_equal(names(caps), c("jpeg","jpegLS","jpeg2000","zlib"))

# Read all
path <- system.file("extdata", "raw", package="divest")
expect_stdout(d <- readDicom(path,interactive=FALSE), "Found 4 DICOM")
expect_stdout(readDicom(path,interactive=FALSE,verbosity=-1), "WARNING")

# Check results
expect_length(d, 2L)
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

jsonString <- paste(readLines("attributes.json"), collapse="\n")
expect_equivalent(jsonlite::fromJSON(jsonString), jsonlite::fromJSON(toBidsJson(attributes[storedAttrNames],rename=FALSE)))
jsonAttributes <- fromBidsJson("attributes.json", rename=TRUE)
expect_equivalent(attributes[storedAttrNames], jsonAttributes[storedAttrNames])
convertedAttributes <- divest:::bidsToDivest(divest:::divestToBids(attributes))
expect_equal(attributes, convertedAttributes)

# Setting `imageAttributes<-`(x,NULL) should scrub all but basic RNifti metadata
copy <- d[[i]]
imageAttributes(copy) <- NULL
expect_equal(attr(d[[i]],"pixdim"), attr(copy,"pixdim"))
expect_null(attr(copy, "echoTime"))

origin <- RNifti::worldToVoxel(c(0,0,0), d[[i]])
expect_equal(round(origin), c(-16,95,135))

# Straight to file conversion
tempDir <- tempdir()
expect_stdout(d <- readDicom(path,output=tempDir), "Found 4 DICOM")
expect_length(d, 2L)
expect_inherits(d, "character")
expect_equal(list.files(tempDir, "\\.nii"), c("T0_N_S8.nii.gz","T0_N_S9.nii.gz"))

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

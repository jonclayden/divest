context("Reading from DICOM files")

test_that("DICOM-reading code works", {
    path <- system.file("extdata", "testdata", package="divest")
    expect_output(d <- readDicom(path,interactive=FALSE), "Found 4 DICOM")
    
    expect_identical(length(d), 2L)
    i <- which(sapply(d,RNifti::ndim) == 3L)
    expect_equal(dim(d[[i]]), c(2,224,256))
    expect_equal(attr(d[[i]],"flipAngle"), 15)
    
    origin <- RNifti::worldToVoxel(c(0,0,0), d[[i]])
    expect_equal(round(origin), c(-16,95,135))
    
    expect_output(d <- readDicom(path,interactive=FALSE,crop=TRUE), "Cropping")
    i <- which(sapply(d,RNifti::ndim) == 3L)
    expect_equal(dim(d[[i]]), c(2,224,170))
    
    expect_output(d <- scanDicom(path), "Found 4 DICOM")
    expect_equal(d$repetitionTime, c(4100,11))
})
context("Reading from DICOM files")

test_that("DICOM-reading code works", {
    path <- system.file("extdata", "testdata", package="divest")
    expect_output(d <- readDicom(path), "Found 4 DICOM")
    
    expect_identical(length(d), 2L)
    expect_equal(dim(d[[1]]), c(2,224,256))
    expect_equal(attr(d[[1]],"flipAngle"), 15)
    
    origin <- RNifti::worldToVoxel(c(0,0,0), d[[1]])
    expect_equal(round(origin), c(-16,95,135))
})

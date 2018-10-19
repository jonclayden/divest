context("Reading from DICOM files")

test_that("DICOM-reading code works", {
    # Read all
    path <- system.file("extdata", "raw", package="divest")
    expect_output(d <- readDicom(path,interactive=FALSE), "Found 4 DICOM")
    expect_output(readDicom(path,interactive=FALSE,verbosity=-1), "Warning")
    
    # Check results
    expect_identical(length(d), 2L)
    i <- which(sapply(d,RNifti::ndim) == 3L)
    expect_equal(dim(d[[i]]), c(2,224,256))
    expect_equal(attr(d[[i]],"flipAngle"), 15)
    
    origin <- RNifti::worldToVoxel(c(0,0,0), d[[i]])
    expect_equal(round(origin), c(-16,95,135))
    
    # Cropping
    expect_output(d <- readDicom(path,interactive=FALSE,crop=TRUE), "Cropping")
    i <- which(sapply(d,RNifti::ndim) == 3L)
    expect_equal(dim(d[[i]]), c(2,224,170))
    
    # Subsets
    expect_output(d <- scanDicom(path), "Found 4 DICOM")
    expect_equal(d$repetitionTime, c(4100,11))
    expect_output(readDicom(d,repetitionTime==4100), "Found 2 DICOM")
    expect_output(readDicom(path,repetitionTime==4100,interactive=FALSE), "Found 2 DICOM")
    
    # Single files
    expect_output(d <- readDicom(file.path(path,"01.dcm"),interactive=FALSE), "Convert 1 DICOM")
    expect_equal(dim(d[[1]]), c(224,256,1))
    expect_warning(readDicom(file.path(path,"nonsense"),interactive=FALSE), "does not exist")
    
    skip_on_cran()
    
    # (Pseudo-)interactivity
    with_mock(`divest:::.readline`=function(...) "1",
        expect_output(d <- readDicom(path,interactive=TRUE), "Found 2 DICOM"),
        expect_equal(unlist(d), "T0_N_S8")
    )
})

test_that("we can read JPEG-encoded data sets", {
    rawpath <- system.file("extdata", "raw", package="divest")
    capture_output(r <- readDicom(rawpath, interactive=FALSE))
    i <- which(sapply(r,RNifti::ndim) == 3L)
    
    path <- system.file("extdata", "jpeg", package="divest")
    expect_output(d <- readDicom(path,interactive=FALSE), "Found 2 DICOM")
    
    expect_identical(length(d), 1L)
    expect_equal(dim(d[[1]]), c(2,224,256))
    expect_equal(attr(d[[1]],"flipAngle"), 15)
    expect_equal(scale(as.array(d[[1]])), scale(as.array(r[[i]])), check.attributes=FALSE, tolerance=0.1)
    
    path <- system.file("extdata", "jpl", package="divest")
    expect_output(d <- readDicom(path,interactive=FALSE), "Found 2 DICOM")
    
    expect_identical(length(d), 1L)
    expect_equal(dim(d[[1]]), c(2,224,256))
    expect_equal(attr(d[[1]],"flipAngle"), 15)
    expect_equal(as.array(d[[1]]), as.array(r[[i]]))
})

test_that("DICOM file sorting works", {
    path <- system.file("extdata", "raw", package="divest")
    temp <- tempdir()
    file.copy(path, temp, recursive=TRUE)
    path <- file.path(temp, "raw")
    
    expect_output(sortDicom(path), "Found 4 DICOM")
    expect_equal(length(list.files(path)), 2L)
    expect_true(all(c("T0_N_S8","T0_N_S9") %in% list.files(path)))
    expect_output(readDicom(file.path(path,"T0_N_S8"),interactive=FALSE), "Found 2 DICOM")
    
    unlink(path, recursive=TRUE)
})

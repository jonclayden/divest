options(divest.bidsAttributes=FALSE)

rawpath <- system.file("extdata", "raw", package="divest")
r <- readDicom(rawpath, interactive=FALSE, verbosity=-2)
i <- which(sapply(r,RNifti::ndim) == 3L)

path <- system.file("extdata", "jpeg", package="divest")
expect_stdout(d <- readDicom(path,interactive=FALSE), "Found 2 DICOM")

expect_identical(length(d), 1L)
expect_equal(dim(d[[1]]), c(2,224,256))
expect_equal(attr(d[[1]],"flipAngle"), 15)
expect_equal(scale(as.array(d[[1]])), scale(as.array(r[[i]])), check.attributes=FALSE, tolerance=0.1)

path <- system.file("extdata", "jpl", package="divest")
expect_stdout(d <- readDicom(path,interactive=FALSE), "Found 2 DICOM")

expect_identical(length(d), 1L)
expect_equal(dim(d[[1]]), c(2,224,256))
expect_equal(attr(d[[1]],"flipAngle"), 15)
expect_equal(as.array(d[[1]]), as.array(r[[i]]))

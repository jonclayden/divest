origPath <- system.file("extdata", "raw", package="divest")
tempPath <- tempdir()
file.copy(origPath, tempPath, recursive=TRUE)
tempPath <- file.path(tempPath, "raw")

expect_stdout(sortDicom(tempPath), "Renamed 4 DICOM")
expect_equal(length(list.files(tempPath)), 2L)
expect_true(all(c("T0_N_S8","T0_N_S9") %in% list.files(tempPath)))
expect_stdout(readDicom(file.path(tempPath,"T0_N_S8"),interactive=FALSE), "Found 2 DICOM")

unlink(tempPath, recursive=TRUE)

dir.create(tempPath)
setwd(tempPath)

expect_stdout(sortDicom(origPath, labelFormat="T%t/S%s/%4r.ima", nested=FALSE, keepUnsorted=TRUE), "Renamed 4 DICOM")
expect_true(all(c("S8","S9") %in% list.files(file.path(tempPath,"T0"))))

unlink(tempPath, recursive=TRUE)

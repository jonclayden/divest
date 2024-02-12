

[![CRAN version](http://www.r-pkg.org/badges/version/divest)](https://cran.r-project.org/package=divest) [![CI Status](https://github.com/jonclayden/divest/actions/workflows/ci.yaml/badge.svg)](https://github.com/jonclayden/divest/actions/workflows/ci.yaml) [![codecov](https://codecov.io/gh/jonclayden/divest/graph/badge.svg?token=515zW7eMSl)](https://app.codecov.io/gh/jonclayden/divest) [![Dependencies](https://tinyverse.netlify.com/badge/divest)](https://tinyverse.netlify.app)

# An R interface to dcm2niix

[DICOM](https://www.dicomstandard.org), for Digital Imaging and Communications in Medicine, is the highly complex standard by which medical imaging devices such as magnetic resonance (MR) and computed tomography (CT) scanners communicate. Importantly for medical imaging research, DICOM defines the format in which images are first created when a subject is scanned. The complexity of DICOM, and the high degree of variation in how it is implemented by hardware vendors, makes it difficult and error-prone to work with. The NIfTI-1 file format has emerged as a simpler, more interoperable standard for medical images, and generally researchers want to convert their images to this format [as soon as possible](https://doi.org/10.1016/j.jneumeth.2016.03.001).

> *divest*, **v.**: rid oneself of something that one no longer wants or requires

The `divest` package is an alternative interface to [Chris Rorden's](http://www.mccauslandcenter.sc.edu/crnl/) excellent [`dcm2niix` DICOM-to-NIfTI conversion tool](https://github.com/rordenlab/dcm2niix). Code has been contributed to `dcm2niix` to support an in-memory interface that links that tool's speed and reliability to the R-native NIfTI tools provided by the [`RNifti` package](https://github.com/jonclayden/RNifti).

The package is [on CRAN](https://cran.r-project.org/package=divest), and the latest development version of the package can always be installed from GitHub using the `remotes` package.


```r
# install.packages("remotes")
remotes::install_github("jonclayden/divest")
```

**Please note that, like `dcm2niix`, the `divest` package is to be used for research purposes only, and is not a clinical tool. It comes with no warranty.**

## Usage

The package's key function is `readDicom`, which scans a directory containing DICOM files, stacks related data into merged 3D or 4D images where appropriate, and returns a list of `niftiImage` objects. For example,


```r
library(divest)
path <- system.file("extdata", "raw", package="divest")
images <- readDicom(path, interactive=FALSE, verbosity=-1)
```

The conversion is interactive by default, prompting the user to select which series to convert, but here we simply convert everything non-interactively. The minimal test dataset provided with the package contains two images from each of two acquisitions. (It is incomplete, hence the warnings.) We can see the basic properties of a converted composite image by printing it.


```r
# Extract the image with a fourth dimension
i <- which(sapply(images, RNifti::ndim) == 4)
images[[i]]
## Internal image: "T0_N_S8"
## - 96 x 96 x 1 x 2 voxels
## - 2.5 x 2.5 x 5 mm x 4.1 s per voxel
```

Additional properties of the scanning sequence, such as the magnetic field strength used, are stored in attributes if they can be deduced from the DICOM files.


```r
imageAttributes(images[[i]])
## $bValues
## [1] 2000 2000
## 
## $bVectors
##        [,1]   [,2]   [,3]
## [1,] 0.5508 0.4258 0.7177
## [2,] 0.1110 0.2640 0.9581
## 
## $modality
## [1] "MR"
## 
## $fieldStrength
## [1] 1.494
## 
## $imagingFrequency
## [1] 63.68285
## 
## $patientPosition
## [1] "HFS"
## 
## $MRAcquisitionType
## [1] "2D"
## 
## $seriesDescription
## [1] "DTIb3000s5"
## 
## $protocolName
## [1] "DTIb3000s5"
## 
## $scanningSequence
## [1] "SE\\EP"
## 
## $sequenceVariant
## [1] "SK\\SP"
## 
## $scanOptions
## [1] "FS"
## 
## $sequenceName
## [1] "ep_b2000#16"
## 
## $imageType
## [1] "ORIGINAL" "PRIMARY"  "M"        "ND"       "NORM"    
## 
## $nonlinearGradientCorrection
## [1] FALSE
## 
## $seriesNumber
## [1] 8
## 
## $acquisitionNumber
## [1] 1
## 
## $sliceThickness
## [1] 5
## 
## $sliceSpacing
## [1] 5
## 
## $SAR
## [1] 0.0973325
## 
## $numberOfAverages
## [1] 2
## 
## $echoTime
## [1] 112
## 
## $repetitionTime
## [1] 4100
## 
## $spoilingState
## [1] TRUE
## 
## $flipAngle
## [1] 90
## 
## $percentPhaseFOV
## [1] 100
## 
## $percentSampling
## [1] 100
## 
## $phaseEncodingSteps
## [1] 72
## 
## $acquisitionMatrixPE
## [1] 96
## 
## $reconMatrixPE
## [1] 96
## 
## $pixelBandwidth
## [1] 900
## 
## $phaseEncodingDirection
## [1] "j"
## 
## $phaseEncodingSign
## [1] -1
## 
## $imageOrientationPatientDICOM
## [1] 1 0 0 0 1 0
## 
## $inPlanePhaseEncodingDirectionDICOM
## [1] "COL"
## 
## $conversionSoftware
## [1] "dcm2niix"
## 
## $conversionSoftwareVersion
## [1] "v1.0.20230904"
```

If desired, functions from the `RNifti` package can be used to inspect and modify the details of the converted NIfTI image, or to write it to file.


```r
library(RNifti)
niftiHeader(images[[i]])
## NIfTI-1 header
##     sizeof_hdr: 348
##       dim_info: 57
##            dim: 4  96  96  1  2  1  1  1
##      intent_p1: 0
##      intent_p2: 0
##      intent_p3: 0
##    intent_code: 0 (Unknown)
##       datatype: 4 (INT16)
##         bitpix: 16
##    slice_start: 0
##         pixdim: -1.0  2.5  2.5  5.0  4.1  0.0  0.0  0.0
##     vox_offset: 352
##      scl_slope: 1
##      scl_inter: 0
##      slice_end: 0
##     slice_code: 0 (Unknown)
##     xyzt_units: 10
##        cal_max: 0
##        cal_min: 0
## slice_duration: 0
##        toffset: 0
##        descrip: TE=1.1e+02;Time=0.000;phase=1
##       aux_file: 
##     qform_code: 1 (Scanner Anat)
##     sform_code: 1 (Scanner Anat)
##      quatern_b: 0
##      quatern_c: 1
##      quatern_d: 0
##      qoffset_x: 122.0339
##      qoffset_y: -101.2288
##      qoffset_z: -55.42373
##         srow_x: -2.5000  0.0000  0.0000  122.0339
##         srow_y: 0.0000  2.5000  0.0000  -101.2288
##         srow_z: 0.00000  0.00000  5.00000  -55.42373
##    intent_name: 
##          magic: n+1
```

```r
writeNifti(images[[i]], "stack")
```

It is also possible to obtain information about the available DICOM series without actually performing the conversion. The `scanDicom` function returns a data frame containing certain information about each series.


```r
names(scanDicom(path))
## [dcm2niix info] Found 4 DICOM file(s)
##  [1] "label"             "rootPath"          "files"            
##  [4] "seriesNumber"      "seriesDescription" "patientName"      
##  [7] "studyDate"         "echoTime"          "repetitionTime"   
## [10] "echoNumber"        "phase"             "diffusion"
```

Elements of this data frame which can't be determined from the DICOM metadata, for example due to anonymisation, will take the conventional `NA` value to indicate missing data.

DICOM files can be converted to NIfTI files on disk, rather than in memory, by using `convertDicom` rather than `readDicom` (or just setting the `output` option):


```r
paths <- convertDicom(path, output=".", interactive=FALSE, verbosity=-1)
```

```r
list.files(pattern="\\.nii")
## [1] "T0_N_S8.nii.gz" "T0_N_S9.nii.gz"
```

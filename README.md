

[![CRAN version](http://www.r-pkg.org/badges/version/divest)](https://cran.r-project.org/package=divest) [![Build Status](https://travis-ci.org/jonclayden/divest.svg?branch=master)](https://travis-ci.org/jonclayden/divest) [![Build status](https://ci.appveyor.com/api/projects/status/lc8v02uc1ywkskrc?svg=true)](https://ci.appveyor.com/project/jonclayden/divest) [![Coverage Status](https://coveralls.io/repos/github/jonclayden/divest/badge.svg?branch=master)](https://coveralls.io/github/jonclayden/divest?branch=master) [![Dependencies](https://tinyverse.netlify.com/badge/divest)](https://tinyverse.netlify.app)

# An R interface to dcm2niix

[DICOM](https://www.dicomstandard.org), for Digital Imaging and Communications in Medicine, is the highly complex standard by which medical imaging devices such as magnetic resonance (MR) and computed tomography (CT) scanners communicate. Importantly for medical imaging research, DICOM defines the format in which images are first created when a subject is scanned. The complexity of DICOM, and the high degree of variation in how it is implemented by hardware vendors, makes it difficult and error-prone to work with. The [NIfTI-1 file format](https://nifti.nimh.nih.gov) has emerged as a simpler, more interoperable standard for medical images, and generally researchers want to convert their images to this format [as soon as possible](https://doi.org/10.1016/j.jneumeth.2016.03.001).

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
## [dcm2niix WARNING] Unable to determine manufacturer (0008,0070), so conversion is not tuned for vendor.
## [dcm2niix WARNING] All images appear to be a single slice - please check slice/vector orientation
## [dcm2niix WARNING] Check that 2D images are not mirrored.
## [dcm2niix WARNING] Unable to determine manufacturer (0008,0070), so conversion is not tuned for vendor.
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
attributes(images[[i]])
## $imagedim
## [1] 96 96  1  2
## 
## $pixdim
## [1] 2.5 2.5 5.0 4.1
## 
## $pixunits
## [1] "mm" "s" 
## 
## $.nifti_image_ptr
## <pointer: 0x7fc594ce4f30>
## 
## $.nifti_image_ver
## [1] 1
## 
## $class
## [1] "internalImage" "niftiImage"   
## 
## $modality
## [1] "MR"
## 
## $imageType
## [1] "ORIGINAL_PRIMARY_M_ND_NORM"
## 
## $seriesNumber
## [1] 8
## 
## $seriesDescription
## [1] "DTIb3000s5"
## 
## $sequenceName
## [1] "ep_b2000#16"
## 
## $protocolName
## [1] "DTIb3000s5"
## 
## $fieldStrength
## [1] 1.494
## 
## $flipAngle
## [1] 90
## 
## $echoTime
## [1] 112
## 
## $repetitionTime
## [1] 4100
## 
## $sliceThickness
## [1] 5
## 
## $sliceSpacing
## [1] 5
## 
## $phaseEncodingSteps
## [1] 72
## 
## $phaseEncodingLines
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

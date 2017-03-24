[![Build Status](https://travis-ci.org/jonclayden/divest.svg?branch=master)](https://travis-ci.org/jonclayden/divest) [![Build status](https://ci.appveyor.com/api/projects/status/lc8v02uc1ywkskrc?svg=true)](https://ci.appveyor.com/project/jonclayden/divest)

# An R interface to dcm2niix

[DICOM](http://dicom.nema.org/), for Digital Imaging and Communications in Medicine, is the highly complex standard by which medical imaging devices such as magnetic resonance (MR) and computed tomography (CT) scanners communicate. Importantly for medical imaging research, DICOM defines the format in which images are first created when a subject is scanned. The complexity of DICOM, and the high degree of variation in how it is implemented by hardware vendors, makes it difficult and error-prone to work with. The [NIfTI-1 file format](https://nifti.nimh.nih.gov) has emerged as a simpler, more interoperable standard for medical images, and generally researchers want to convert their images to this format as soon as possible.

> *divest*, **v.**: rid oneself of something that one no longer wants or requires

The `divest` package is a fork of [Chris Rorden's](http://www.mccauslandcenter.sc.edu/crnl/) excellent [`dcm2niix`](https://github.com/rordenlab/dcm2niix) DICOM-to-NIfTI conversion tool, which has been minimally restructured to support an in-memory interface to R. It links the speed and reliability of the popular `dcm2niix` to the R-native NIfTI tools provided by the [`RNifti` package](https://github.com/jonclayden/RNifti).

The latest development version of the package can always be installed from GitHub using the `devtools` package.

```r
## install.packages("devtools")
devtools::install_github("jonclayden/divest")
```

**Please note that, like `dcm2niix`, the `divest` package is to be used for research purposes only, and is not a clinical tool. It comes with no warranty.**

## Usage

The package's key function is `readDicom`, which scans a directory containing DICOM files, stacks related data into merged 3D or 4D images where appropriate, and returns a list of `niftiImage` objects. For example,

```r
library(divest)
path <- system.file("extdata", "testdata", package="divest")
images <- readDicom(path)
# [dcm2niix info] Found 4 DICOM image(s)
# 
# 1: Series 8 "DTIb3000s5", TE 112 ms, TR 4100 ms 
# 2: Series 9 "fl3D_t1_sag", TE 4.94 ms, TR 11 ms
# 
# Type <Enter> for all series, 0 for none, or indices separated by spaces or commas
# Selected series: 
```

Notice that the conversion is interactive by default, prompting the user to select which series to convert. The minimal test dataset provided with the package contains two images from each of two acquisitions, and so pressing `<Enter>` will result in a list with two elements. We can see the basic properties of the first by printing it.

```r
images[[1]]
# Internal image: "T0_N_S8"
# - 96 x 96 x 1 x 2 voxels
# - 2.5 x 2.5 x 5 mm x 4.1 s per voxel
```

Additional properties of the scanning sequence, such as the magnetic field strength used, are stored in attributes if they can be deduced from the DICOM files.

```r
attributes(images[[1]])
# $imagedim
# [1] 96 96  1  2
# 
# $pixdim
# [1] 2.5 2.5 5.0 4.1
# 
# $pixunits
# [1] "mm" "s" 
# 
# $.nifti_image_ptr
# <pointer: 0x7ff1726114c0>
# 
# $class
# [1] "internalImage" "niftiImage"
# 
# $imageType
# [1] "ORIGINAL_PRIMARY_M_ND_NORM"
# 
# $fieldStrength
# [1] 1.494
# 
# $flipAngle
# [1] 90
# 
# $echoTime
# [1] 112
# 
# $repetitionTime
# [1] 4100
# 
# $phaseEncodingDirection
# [1] "j"
# 
# $phaseEncodingSign
# [1] 1
```

If desired, functions from the `RNifti` package can be used to inspect and modify the details of the converted NIfTI image, or to write it to file.

```r
library(RNifti)
dumpNifti(images[[1]])
# NIfTI-1 header
#     sizeof_hdr: 348
#       dim_info: 57
#            dim: 4  96  96  1  2  1  1  1
#      intent_p1: 0
#      intent_p2: 0
#      intent_p3: 0
#    intent_code: 0
#       datatype: 4
#         bitpix: 16
#    slice_start: 0
#         pixdim: -1.0  2.5  2.5  5.0  4.1  0.0  0.0  0.0
#     vox_offset: 352
#      scl_slope: 1
#      scl_inter: 0
#      slice_end: 0
#     slice_code: 0
#     xyzt_units: 10
#        cal_max: 0
#        cal_min: 0
# slice_duration: 0
#        toffset: 0
#        descrip: TE=1.1e+02;Time=0.000;phase=1
#       aux_file: imgComments
#     qform_code: 1
#     sform_code: 1
#      quatern_b: 0
#      quatern_c: 1
#      quatern_d: 0
#      qoffset_x: 122.0339
#      qoffset_y: -101.2288
#      qoffset_z: -55.42373
#         srow_x: -2.5000  0.0000  0.0000  122.0339
#         srow_y: 0.0000  2.5000  0.0000  -101.2288
#         srow_z: 0.00000  0.00000  5.00000  -55.42373
#    intent_name: 
#          magic: n+1

writeNifti(images[[1]], "stack")
```

It is also possible to obtain information about the available DICOM series without actually performing the conversion. The `scanDicom` function returns a data frame containing certain information about each series.

```r
names(scanDicom(path))
# [dcm2niix info] Found 4 DICOM image(s)
# [1] "rootPath"          "seriesNumber"      "seriesDescription"
# [4] "patientName"       "studyDate"         "echoTime"
# [7] "repetitionTime"    "echoNumber"        "phase"
```

Elements of this data frame which can't be determined from the DICOM metadata, for example due to anonymisation, will take the conventional `NA` value to indicate missing data.

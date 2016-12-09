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
```

The minimal test dataset provided with the package contains two images from each of two acquisitions, and so the result will be a list with two elements. We can see the basic properties of the first by printing it.

```r
images[[1]]
# Internal image: "T0_N_S9"
# - 2 x 224 x 256 voxels
# - 1 x 1 x 1 mm per voxel
```

Additional properties of the scanning sequence, such as the magnetic field strength used, are stored in attributes if they can be deduced from the DICOM files.

```r
attributes(images[[1]])
# $imagedim
# [1]  2 224 256
# 
# $pixdim
# [1] 1 1 1
# 
# $pixunits
# [1] "mm" "s" 
# 
# $.nifti_image_ptr
# <pointer: 0x7ff8fff81bf0>
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
# [1] 15
# 
# $echoTime
# [1] 4.94
# 
# $repetitionTime
# [1] 11
# 
# $phaseEncodingDirection
# [1] "i"
```

If desired, functions from the `RNifti` package can be used to inspect and modify the details of the NIfTI image, and to write it to file.

```r
library(RNifti)
dumpNifti(images[[1]])
# NIfTI-1 header
#     sizeof_hdr: 348
#       dim_info: 54
#            dim: 3  2  224  256  1  1  1  1
#      intent_p1: 0
#      intent_p2: 0
#      intent_p3: 0
#    intent_code: 0
#       datatype: 4
#         bitpix: 16
#    slice_start: 0
#         pixdim: 1.000  1.000  1.000  1.000  0.011  0.000  0.000  0.000
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
#        descrip: TE=4.9;Time=0.000;phase=1
#       aux_file: imgComments
#     qform_code: 1
#     sform_code: 1
#      quatern_b: 0
#      quatern_c: 0
#      quatern_d: 0
#      qoffset_x: 17.24576
#      qoffset_y: -94.05084
#      qoffset_z: -133.7797
#         srow_x: 1.00000  0.00000  0.00000  17.24576
#         srow_y: 0.00000  1.00000  0.00000  -94.05084
#         srow_z: 0.0000  0.0000  1.0000  -133.7797
#    intent_name: 
#          magic: n+1

writeNifti(images[[1]], "stack")
```

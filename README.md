# An R interface to dcm2niix

[DICOM](http://dicom.nema.org/), for Digital Imaging and Communications in Medicine, is the highly complex standard by which medical imaging devices such as magnetic resonance (MR) and computed tomography (CT) scanners communicate. Importantly for medical imaging research, DICOM defines the format in which images are first created when a subject is scanned. The complexity of DICOM, and the high degree of variation in how it is implemented by hardware vendors, makes it difficult and error-prone to work with. The [NIfTI-1 file format](https://nifti.nimh.nih.gov) has emerged as a simpler, more interoperable standard for medical images, and generally researchers want to convert their images to this format as soon as possible.

> *divest*, **v.**: rid oneself of something that one no longer wants or requires

The `divest` package is a fork of [Chris Rorden's](http://www.mccauslandcenter.sc.edu/crnl/) excellent [`dcm2niix`](https://github.com/rordenlab/dcm2niix) DICOM-to-NIfTI conversion tool, which has been minimally restructured to support an in-memory interface to R. It links the speed and reliability of the popular `dcm2niix` to the R-native NIfTI tools provided by the [`RNifti` package](https://github.com/jonclayden/RNifti).

The latest development version of the package can always be installed from GitHub using the `devtools` package.

```r
## install.packages("devtools")
devtools::install_github("jonclayden/divest")
```

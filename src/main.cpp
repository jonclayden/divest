#include <Rcpp.h>

#include "RNifti.h"
#include "RNiftiAPI.h"

#include "ImageList.h"
#include "nii_dicom_batch.h"
#include "nii_dicom.h"

using namespace Rcpp;

RcppExport SEXP initialise ()
{
BEGIN_RCPP
    niftilib_register_all();
END_RCPP
}

RcppExport SEXP readDirectory (SEXP path_, SEXP flipY_, SEXP verbosity_)
{
BEGIN_RCPP
    const std::string path = as<std::string>(path_);
    
    TDCMopts options;
    options.isGz = false;
    options.isFlipY = as<bool>(flipY_);
    options.isCreateBIDS = false;
    options.isCreateText = false;
    options.isTiltCorrect = false;
    options.isRGBplanar = false;
    options.isOnlySingleFile = false;
    options.isForceStackSameSeries = false;
    options.isCrop = false;
    options.isVerbose = as<int>(verbosity_);
    options.compressFlag = 0;
    strcpy(options.indir, path.c_str());
    strcpy(options.outdir, "");
    strcpy(options.filename, "");
    strcpy(options.pigzname, "");
    
    ImageList images;
    options.imageList = (void *) &images;
    
    int returnValue = nii_loadDir(&options);
    if (returnValue == EXIT_SUCCESS)
        return images;
    else
        return R_NilValue;
END_RCPP
}

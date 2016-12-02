#include <Rcpp.h>

#include "RNifti.h"
#include "RNiftiAPI.h"

#include "ImageList.h"
#include "nii_dicom_batch.h"
#include "nii_dicom.h"

RcppExport SEXP initialise ()
{
BEGIN_RCPP
    niftilib_register_all();
END_RCPP
}

RcppExport SEXP readDirectory (SEXP path_)
{
BEGIN_RCPP
    const std::string path = Rcpp::as<std::string>(path_);
    
    struct TDCMopts options;
    options.isGz = false;
    options.isFlipY = true;
    options.isCreateBIDS = false;
    options.isCreateText = false;
    options.isTiltCorrect = false;
    options.isRGBplanar = false;
    options.isOnlySingleFile = false;
    options.isForceStackSameSeries = false;
    options.isCrop = false;
    options.isVerbose = 0;
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

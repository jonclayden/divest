#include <Rcpp.h>

#include "RNifti.h"
#include "RNiftiAPI.h"

#include "nii_dicom_batch.h"
#include "nii_dicom.h"

RcppExport SEXP initialise ()
{
BEGIN_RCPP
    niftilib_register_all();
END_RCPP
}

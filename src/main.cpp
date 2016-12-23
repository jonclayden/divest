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

RcppExport SEXP readDirectory (SEXP path_, SEXP flipY_, SEXP verbosity_, SEXP scanOnly_)
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
    options.isScanOnly = as<bool>(scanOnly_);
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
    {
        if (options.isScanOnly)
        {
            List series(options.series.size());
            for (int i = 0; i < options.series.size(); i++)
            {
                const TDICOMdata &data = options.series[i].representativeData;
                std::ostringstream description;
                description << "Series " << data.seriesNum;
                if (strlen(data.seriesDescription) > 0)
                    description << " \"" << data.seriesDescription << "\"";
                if (strlen(data.patientName) > 0)
                    description << ", patient \"" << data.patientName << "\"";
                if (strlen(data.studyDate) > 0 && strcmp(data.studyDate,"00000000") != 0)
                    description << ", acquired on " << data.studyDate;
                if (data.TE > 0.0)
                    description << ", TE " << data.TE << " ms";
                if (data.TR > 0.0)
                    description << ", TR " << data.TR << " ms";
                if (data.echoNum > 1)
                    description << ", echo " << data.echoNum;
                if (data.isHasPhase)
                    description << ", phase";
                RObject element = wrap(description.str());
                element.attr("paths") = wrap(options.series[i].files);
                series[i] = element;
            }
            return series;
        }
        else
            return images;
    }
    else
        return R_NilValue;
END_RCPP
}

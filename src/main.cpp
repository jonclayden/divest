#include <Rcpp.h>

#include "RNifti.h"
#include "RNiftiAPI.h"

#include "ImageList.h"
#include "nii_dicom_batch.h"
#include "nii_dicom.h"

using namespace Rcpp;

RcppExport SEXP readDirectory (SEXP path_, SEXP flipY_, SEXP crop_, SEXP forceStack_, SEXP verbosity_, SEXP labelFormat_, SEXP scanOnly_)
{
BEGIN_RCPP
    const std::string path = as<std::string>(path_);
    const std::string labelFormat = as<std::string>(labelFormat_);
    
    TDCMopts options;
    options.isGz = false;
    options.isFlipY = as<bool>(flipY_);
    options.isCreateBIDS = false;
    options.isCreateText = false;
    options.isTiltCorrect = true;
    options.isRGBplanar = false;
    options.isOnlySingleFile = false;
    options.isForceStackSameSeries = as<bool>(forceStack_);
    options.isIgnoreDerivedAnd2D = false;
    options.isPhilipsFloatNotDisplayScaling = true;
    options.isCrop = as<bool>(crop_);
    options.isScanOnly = as<bool>(scanOnly_);
    options.isVerbose = as<int>(verbosity_);
    options.compressFlag = kCompressYes;
    strcpy(options.indir, path.c_str());
    strcpy(options.outdir, "");
    strcpy(options.filename, labelFormat.c_str());
    strcpy(options.pigzname, "");
    
    ImageList images;
    options.imageList = (void *) &images;
    
    // Scan the directory of interest, and create NiftiImage objects if required
    int returnValue = nii_loadDir(&options);
    if (returnValue == EXIT_SUCCESS)
    {
        if (options.isScanOnly)
        {
            // Construct a data frame containing information about each series
            // A vector of descriptive strings is also built, and attached as an attribute
            const int n = options.series.size();
            CharacterVector seriesDescription(n,NA_STRING), patientName(n,NA_STRING), descriptions(n);
            DateVector studyDate(n);
            NumericVector echoTime(n,NA_REAL), repetitionTime(n,NA_REAL);
            IntegerVector seriesNumber(n,NA_INTEGER), echoNumber(n,NA_INTEGER);
            LogicalVector phase(n,NA_LOGICAL);
            List paths(n);
            for (int i = 0; i < n; i++)
            {
                const TDICOMdata &data = options.series[i].representativeData;
                std::ostringstream description;
                description << "Series " << data.seriesNum;
                seriesNumber[i] = data.seriesNum;
                if (strlen(data.seriesDescription) > 0)
                {
                    description << " \"" << data.seriesDescription << "\"";
                    seriesDescription[i] = data.seriesDescription;
                }
                else if (strlen(data.sequenceName) > 0)
                    description << " \"" << data.sequenceName << "\"";
                else if (strlen(data.protocolName) > 0)
                    description << " \"" << data.protocolName << "\"";
                if (strlen(data.patientName) > 0)
                {
                    description << ", patient \"" << data.patientName << "\"";
                    patientName[i] = data.patientName;
                }
                if (strlen(data.studyDate) >= 8 && strcmp(data.studyDate,"00000000") != 0)
                {
                    description << ", acquired on " << std::string(data.studyDate,4) << "-" << std::string(data.studyDate+4,2) << "-" << std::string(data.studyDate+6,2);
                    studyDate[i] = Date(data.studyDate, "%Y%m%d");
                }
                else
                    studyDate[i] = Date(NA_REAL);
                if (data.TE > 0.0)
                {
                    description << ", TE " << data.TE << " ms";
                    echoTime[i] = data.TE;
                }
                if (data.TR > 0.0)
                {
                    description << ", TR " << data.TR << " ms";
                    repetitionTime[i] = data.TR;
                }
                if (data.echoNum > 0)
                    echoNumber[i] = data.echoNum;
                if (data.echoNum > 1)
                    description << ", echo " << data.echoNum;
                if (data.isHasPhase)
                    description << ", phase";
                
                phase[i] = data.isHasPhase;
                descriptions[i] = description.str();
                paths[i] = wrap(options.series[i].files);
            }
            
            DataFrame info = DataFrame::create(Named("rootPath")=path, Named("seriesNumber")=seriesNumber, Named("seriesDescription")=seriesDescription, Named("patientName")=patientName, Named("studyDate")=studyDate, Named("echoTime")=echoTime, Named("repetitionTime")=repetitionTime, Named("echoNumber")=echoNumber, Named("phase")=phase, Named("stringsAsFactors")=false);
            info.attr("descriptions") = descriptions;
            info.attr("paths") = paths;
            info.attr("class") = CharacterVector::create("divest","data.frame");
            return info;
        }
        else
            return images;
    }
    else
        Rf_error("DICOM scan failed");
END_RCPP
}

static const R_CallMethodDef callMethods[] = {
  { "readDirectory", (DL_FUNC) &readDirectory, 7 },
  { NULL, NULL, 0 }
};

extern "C" {

void R_init_divest (DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
    R_forceSymbols(info, TRUE);
    
    niftilib_register_all();
}

}

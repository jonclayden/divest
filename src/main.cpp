#include <Rcpp.h>

#include "RNifti.h"
#include "RNiftiAPI.h"

#include "ImageList.h"
#include "nii_dicom_batch.h"
#include "nii_dicom.h"

using namespace Rcpp;

RcppExport SEXP readDirectory (SEXP path_, SEXP flipY_, SEXP crop_, SEXP forceStack_, SEXP verbosity_, SEXP labelFormat_, SEXP singleFile_, SEXP depth_, SEXP task_, SEXP outputDir_)
{
BEGIN_RCPP
    const std::string path = as<std::string>(path_);
    const std::string labelFormat = as<std::string>(labelFormat_);
    const std::string task = as<std::string>(task_);
    
    TDCMopts options;
    setDefaultOpts(&options, NULL);
    options.isGz = false;
    options.gzLevel = 0;
    options.isFlipY = as<bool>(flipY_);
    options.isCreateBIDS = false;
    options.isCreateText = false;
    options.isSortDTIbyBVal = false;
    options.isForceStackSameSeries = as<bool>(forceStack_);
    options.isCrop = as<bool>(crop_);
    options.isOnlySingleFile = as<bool>(singleFile_);
    options.isScanOnly = (task == "scan");
    options.isRenameNotConvert = (task == "sort");
    options.isVerbose = as<int>(verbosity_);
    options.dirSearchDepth = as<int>(depth_);
    options.compressFlag = kCompressYes;
    strcpy(options.indir, path.c_str());
    strcpy(options.filename, labelFormat.c_str());
    if (task == "sort" && !Rf_isNull(outputDir_))
    {
        const std::string outputDir = as<std::string>(outputDir_);
        strcpy(options.outdir, outputDir.c_str());
    }
    
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
            CharacterVector label(n,NA_STRING), seriesDescription(n,NA_STRING), patientName(n,NA_STRING), descriptions(n);
            DateVector studyDate(n);
            NumericVector echoTime(n,NA_REAL), repetitionTime(n,NA_REAL);
            IntegerVector files(n,NA_INTEGER), seriesNumber(n,NA_INTEGER), echoNumber(n,NA_INTEGER);
            LogicalVector phase(n,NA_LOGICAL), diffusion(n,false);
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
                if (data.CSA.numDti > 0)
                    diffusion[i] = true;
                
                // The name is stored with leading path components, which we remove here
                if (options.series[i].name.length() > 0)
                {
#if defined(_WIN32) || defined(_WIN64)
                    size_t pathSeparator = options.series[i].name.find_last_of("\\/");
#else
                    size_t pathSeparator = options.series[i].name.find_last_of('/');
#endif
                    if (pathSeparator == std::string::npos)
                        label[i] = options.series[i].name;
                    else
                        label[i] = options.series[i].name.substr(pathSeparator+1);
                }
                
                phase[i] = data.isHasPhase;
                descriptions[i] = description.str();
                files[i] = options.series[i].files.size();
                paths[i] = wrap(options.series[i].files);
            }
            
            DataFrame info = DataFrame::create(Named("label")=label, Named("rootPath")=path, Named("files")=files, Named("seriesNumber")=seriesNumber, Named("seriesDescription")=seriesDescription, Named("patientName")=patientName, Named("studyDate")=studyDate, Named("echoTime")=echoTime, Named("repetitionTime")=repetitionTime, Named("echoNumber")=echoNumber, Named("phase")=phase, Named("diffusion")=diffusion, Named("stringsAsFactors")=false);
            info.attr("descriptions") = descriptions;
            info.attr("paths") = paths;
            info.attr("class") = CharacterVector::create("divest","data.frame");
            return info;
        }
        else if (options.isRenameNotConvert)
            return List::create(Named("source")=options.sourcePaths, Named("target")=options.targetPaths, Named("ignored")=options.ignoredPaths);
        else
            return images;
    }
    else if (returnValue == kEXIT_NO_VALID_FILES_FOUND)
        Rprintf("No valid DICOM files found\n");
    else
        Rf_error("DICOM scan failed");
END_RCPP
}

static const R_CallMethodDef callMethods[] = {
  { "readDirectory", (DL_FUNC) &readDirectory, 10 },
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

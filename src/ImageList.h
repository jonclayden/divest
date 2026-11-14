#ifndef _IMAGE_LIST_H_
#define _IMAGE_LIST_H_

#ifdef USING_R

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "RNifti.h"
#include "nii_dicom.h"

struct TDicomSeries {
    std::string name;
    TDICOMdata representativeData;
    std::vector<std::string> files;
};

class ImageList
{
private:
    // BIDS JSON buffer size limit per image, currently 4 MiB
    const size_t jsonBufferSize = 0x400000;
    char *jsonBuffer;
    FILE *jsonHandle_;
    
    std::vector<std::string> paths_;
    
    Rcpp::List list;
    Rcpp::List deferredAttributes;
    
public:
    // Results of scanning or sorting a directory, which are kept here rather
    // than in TDCMopts, since dcm2niix often copies the latter
    std::vector<TDicomSeries> series;
    std::vector<std::string> sourcePaths, targetPaths, ignoredPaths;
    
    ImageList () : jsonBuffer(NULL), jsonHandle_(NULL) {}
    
    operator SEXP ()
    {
        return list;
    }
    
    Rcpp::CharacterVector pathVector ()
    {
        return Rcpp::wrap(paths_);
    }
    
    FILE * jsonHandle ()
    {
#ifdef HAVE_FMEMOPEN
        // If the system supports it, use fmemopen to stream into a string buffer
        // This is C style, but dcm2niix expects a FILE*
        // Each sidecar gets a fresh buffer: any existing handle belongs to a
        // sidecar that was never attached to an image, and dcm2niix will
        // already have closed it, so it must not be reused
        jsonBuffer = R_alloc(jsonBufferSize, 1);
        jsonHandle_ = fmemopen(jsonBuffer, jsonBufferSize, "w");
#endif
        return jsonHandle_;
    }

    // Attach a pending sidecar to the most recently appended image, for cases
    // (such as MR spectroscopy) where dcm2niix writes the sidecar afterwards
    void attachJson ()
    {
        if (jsonHandle_ != NULL && list.size() > 0)
        {
            Rcpp::RObject pointer = list[list.size() - 1];
            pointer.attr(".bidsJson") = const_cast<const char *>(jsonBuffer);
        }
        jsonHandle_ = NULL;
    }
    
    void append (nifti_image * const image, const std::string &name)
    {
        RNifti::NiftiImage wrapper(image, true);
        Rcpp::RObject pointer = wrapper.toPointer(name);
        std::vector<std::string> classStrings = pointer.attr("class");
        classStrings.insert(classStrings.begin(), "divestImage");
        pointer.attr("class") = classStrings;
        
        if (deferredAttributes.size() > 0)
        {
            std::vector<std::string> attributeNames = deferredAttributes.names();
            for (int i = 0; i < deferredAttributes.size(); i++)
                pointer.attr(attributeNames[i]) = deferredAttributes[i];
            deferredAttributes = Rcpp::List();
        }
        
        if (jsonHandle_ != NULL)
        {
            // dcm2niix handles closing the file handle
            pointer.attr(".bidsJson") = const_cast<const char *>(jsonBuffer);
            jsonHandle_ = NULL;
        }
        
        list.push_back(pointer);
    }
    
    template <typename ValueType>
    void addDeferredAttribute (const std::string &name, const ValueType &value)
    {
        deferredAttributes[name] = value;
    }
    
    template <typename ValueType>
    void addDeferredAttribute (const std::string &name, const ValueType &value, const int nRows, const int nCols)
    {
        Rcpp::RObject wrappedValue = Rcpp::wrap(value);
        wrappedValue.attr("dim") = Rcpp::Dimension(nRows, nCols);
        deferredAttributes[name] = wrappedValue;
    }
    
    void appendPath (const std::string &path)
    {
        paths_.push_back(path);
    }
};

#endif

#endif

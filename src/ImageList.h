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
    // BIDS JSON sidecar buffer, allocated by open_memstream() and valid once
    // dcm2niix has closed the handle
    char *jsonBuffer;
    size_t jsonSize;
    FILE *jsonHandle_;
    
    std::vector<std::string> paths_;
    
    // Images are collected in a vector, since Rcpp::List::push_back() copies
    // the whole list each time; the RObject wrappers protect them from R's GC
    std::vector<Rcpp::RObject> images_;
    Rcpp::List deferredAttributes;
    
    void freeJson ()
    {
        free(jsonBuffer);
        jsonBuffer = NULL;
        jsonSize = 0;
    }
    
public:
    // Results of scanning or sorting a directory, which are kept here rather
    // than in TDCMopts, since dcm2niix often copies the latter
    std::vector<TDicomSeries> series;
    std::vector<std::string> sourcePaths, targetPaths, ignoredPaths;
    
    ImageList ()
        : jsonBuffer(NULL), jsonSize(0), jsonHandle_(NULL) {}
    
    ~ImageList ()
    {
        freeJson();
    }
    
    // Copying would lead to the buffer being freed twice
    ImageList (const ImageList &) = delete;
    ImageList & operator= (const ImageList &) = delete;
    
    operator SEXP ()
    {
        return Rcpp::List(images_.begin(), images_.end());
    }
    
    Rcpp::CharacterVector pathVector ()
    {
        return Rcpp::wrap(paths_);
    }
    
    FILE * jsonHandle ()
    {
#ifdef HAVE_OPEN_MEMSTREAM
        // If the system supports it, stream into a buffer that grows as needed
        // This is C style, but dcm2niix expects a FILE*
        // Each sidecar gets a fresh buffer: any existing one belongs to a
        // sidecar that was never attached to an image, and dcm2niix will
        // already have closed its handle, so it can be released
        jsonHandle_ = NULL;
        freeJson();
        jsonHandle_ = open_memstream(&jsonBuffer, &jsonSize);
#endif
        return jsonHandle_;
    }

    // Attach the pending sidecar, if any, to the most recently appended image,
    // and release its buffer; this is called by append(), and separately for
    // cases (such as MR spectroscopy) where dcm2niix writes the sidecar
    // afterwards
    void attachJson ()
    {
        if (!images_.empty() && jsonHandle_ != NULL && jsonBuffer != NULL)
            images_.back().attr(".bidsJson") = const_cast<const char *>(jsonBuffer);
        
        jsonHandle_ = NULL;
        freeJson();
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
        
        images_.push_back(pointer);
        attachJson();
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

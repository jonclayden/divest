#ifndef _IMAGE_LIST_H_
#define _IMAGE_LIST_H_

#ifdef USING_R

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "RNifti.h"

class ImageList
{
private:
    // BIDS JSON buffer size limit per image, currently 4 MiB
    const size_t jsonBufferSize = 0x400000;
    char *jsonBuffer;
    FILE *jsonHandle_;
    
    Rcpp::List list;
    Rcpp::List deferredAttributes;
    
public:
    ImageList () : jsonBuffer(NULL), jsonHandle_(NULL) {}
    
    operator SEXP ()
    {
        return list;
    }
    
    FILE * jsonHandle ()
    {
#ifdef HAVE_FMEMOPEN
        if (jsonHandle_ == NULL)
        {
            // If the system supports it, use fmemopen to stream into a string buffer
            // This is C style, but dcm2niix expects a FILE*
            jsonBuffer = R_alloc(jsonBufferSize, 1);
            jsonHandle_ = fmemopen(jsonBuffer, jsonBufferSize, "w");
        }
#endif
        return jsonHandle_;
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
};

#endif

#endif

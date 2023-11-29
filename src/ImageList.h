#ifndef _IMAGE_LIST_H_
#define _IMAGE_LIST_H_

#ifdef USING_R

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "RNifti.h"

namespace internal {

template <class Type>
inline bool validAttribute (const Type &value)    { return true; }

template <>
inline bool validAttribute (const float &value)   { return value > 0.0; }

template <>
inline bool validAttribute (const double &value)  { return value > 0.0; }

}

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
    void addAttribute (const std::string &name, const ValueType &value)
    {
        if (internal::validAttribute(value))
        {
            Rcpp::RObject element = list[list.length()-1];
            element.attr(name) = value;
        }
    }
    
    void addAttribute (const std::string &name, char *value)
    {
        char *ptr = value;
        bool empty = true;
        while (*ptr != '\0')
        {
            // Strings composed entirely of spaces are considered empty and not stored
            if (*ptr != ' ')
            {
                empty = false;
                break;
            }
            ptr++;
        }
        if (!empty)
        {
            Rcpp::RObject element = list[list.length()-1];
            element.attr(name) = const_cast<const char *>(value);
        }
    }
    
    void addDateAttribute (const std::string &name, const char *value)
    {
        // DICOM data format is YYYYMMDD; empty values are zero-filled
        if (strlen(value) == 8 && strcmp(value,"00000000") != 0)
        {
            Rcpp::RObject element = list[list.length()-1];
            element.attr(name) = Rcpp::Date(value, "%Y%m%d");
        }
    }
    
    void addTimeAttribute (const std::string &name, const char *value)
    {
        // DICOM time format is HHMMSS.FFFFFF; empty values are zero-filled
        if (strlen(value) == 13 && strcmp(value,"000000.000000") != 0)
        {
            Rcpp::RObject element = list[list.length()-1];
            element.attr(name) = value;
        }
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

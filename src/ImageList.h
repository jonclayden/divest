#ifndef _IMAGE_LIST_H_
#define _IMAGE_LIST_H_

#ifdef USING_R

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "RNifti.h"

class ImageList
{
private:
    Rcpp::List list;
    Rcpp::List deferredAttributes;
    
public:
    operator SEXP ()
    {
        return list;
    }
    
    void append (nifti_image * const image, const std::string &name)
    {
        RNifti::NiftiImage wrapper(image, true);
        wrapper.setPersistence(true);
        Rcpp::RObject pointer = wrapper.toPointer(name);
        
        if (deferredAttributes.size() > 0)
        {
            std::vector<std::string> attributeNames = deferredAttributes.names();
            for (int i = 0; i < deferredAttributes.size(); i++)
                pointer.attr(attributeNames[i]) = deferredAttributes[i];
            deferredAttributes = Rcpp::List();
        }
        
        list.push_back(pointer);
    }
    
    template <typename ValueType>
    void addAttribute (const std::string &name, const ValueType &value)
    {
        Rcpp::RObject element = list[list.length()-1];
        element.attr(name) = value;
    }
    
    template <>
    void addAttribute (const std::string &name, const float &value)
    {
        if (value > 0.0)
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

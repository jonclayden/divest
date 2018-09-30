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
    
    void addDateAttribute (const std::string &name, const char *value)
    {
        Rcpp::RObject element = list[list.length()-1];
        element.attr(name) = Rcpp::Date(value, "%Y%m%d");
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

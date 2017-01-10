#ifndef _IMAGE_LIST_H_
#define _IMAGE_LIST_H_

#ifdef HAVE_R

#define STRICT_R_HEADERS
#include <Rcpp.h>

#include "RNifti.h"

class ImageList
{
private:
    Rcpp::List list;
    
public:
    operator SEXP ()
    {
        return list;
    }
    
    void append (nifti_image * const image, const std::string &name)
    {
        RNifti::NiftiImage wrapper(image);
        wrapper.setPersistence(true);
        list.push_back(wrapper.toPointer(name));
    }
    
    template <typename ValueType>
    void addAttribute (const std::string &name, const ValueType &value)
    {
        Rcpp::RObject element = list[list.length()-1];
        element.attr(name) = value;
    }
    
    template <typename ValueType>
    void addAttribute (const std::string &name, const ValueType &value, const int nRows, const int nCols)
    {
        Rcpp::RObject element = list[list.length()-1];
        element.attr(name) = value;
        Rcpp::RObject(element.attr(name)).attr("dim") = Rcpp::Dimension(nRows, nCols);
    }
};

#endif

#endif

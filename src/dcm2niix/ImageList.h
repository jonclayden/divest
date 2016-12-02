#ifndef _IMAGE_LIST_H_
#define _IMAGE_LIST_H_

#ifdef HAVE_R

#include <Rcpp.h>

#include "RNifti.h"

class ImageList
{
private:
    Rcpp::List list;
    
public:
    operator SEXP()
    {
        return list;
    }
    
    void append (nifti_image * const image)
    {
        NiftiImage wrapper(image);
        wrapper.setPersistence(true);
        list.push_back(wrapper.toPointer("Converted DICOM series"));
    }
};

#endif

#endif

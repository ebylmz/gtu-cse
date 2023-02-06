#ifndef ARRAY_WRAPPER_H
#define ARRAY_WRAPPER_H

#include <iostream>
#include <string>
#include "metaData.h"

namespace ARR {
    class ArrayWrapper {
    public:
        ArrayWrapper (int n = 64); 
        // copy constructor
        ArrayWrapper (const ArrayWrapper & other);  
        // move constructor
        ArrayWrapper (ArrayWrapper && other);       
        // destructure
        ~ArrayWrapper ();
    private:
        int * _p_vals;
        MetaData _metadata;
    };
}

#endif
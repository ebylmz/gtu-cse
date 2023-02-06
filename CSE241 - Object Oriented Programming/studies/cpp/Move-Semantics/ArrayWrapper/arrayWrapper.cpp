#include <iostream>
#include <string>
#include "arrayWrapper.h"

// copy constructors can reach all the data of the class regardless private 
// or public otherwise how we can copy all the data from other to one

// std::move() converts a lvalue to rvalue

//! also functions can overload as move reference
// for example std::vector::push_back
// void push_back(const value_type & val);
// void push_back(const value_type && val);

namespace ARR {
    ArrayWrapper::ArrayWrapper (int n) 
    : _p_vals(new int[n]), 
      _metadata(n, "ArrayWrapper") {}

    ArrayWrapper::ArrayWrapper (const ArrayWrapper & other)
    : _p_vals(new int[other._metadata.size()]),
      _metadata(std::move(other._metadata)) {
        // since other._metadata is lvalue this move constructor uses copy constructor which is so bad
        // so solution is telling compiler to set _metadata as rvalue
        //   https://youtu.be/H_sPdFoBQ6Y?list=PLuid2q5iknODFPHJPoO0gE1aWL9LGnqZ1&t=4998
        for (int i = 0; i < _metadata.size(); ++i)
            _p_vals[i] = other._p_vals[i];
    }

    ArrayWrapper::ArrayWrapper (ArrayWrapper && other) 
    : _p_vals(other._p_vals), _metadata(other._metadata) {
        // It can be destroyed without deleting my resource by destructure
        other._p_vals = NULL;
    }

    ArrayWrapper::~ArrayWrapper () {
        delete[] _p_vals;
    }
}
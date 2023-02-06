#ifndef ARRAY_H
#define ARRAY_H

#include <iostream>
#include <exception>

using std::out_of_range;

namespace stdEBY {
    template<class T, size_t N>
    class Array {
    public:
        Array ();
        ~Array ();
        //! Iterators
        //! not Implemented yet

        int size () const;
        bool empty () const;

        T & operator[] (int i) noexcept;
        const T & operator[] (int i) const noexcept;

        T & at (int i) throw (out_of_range);
        const T & at (int i) const throw (out_of_range);

        // fills the array with given value
        void fill (const T & value);
        // fills the array with given value
        void fill (T && value);
    private:
        T * _arr;
    };
}

#include "array.cpp"

#endif
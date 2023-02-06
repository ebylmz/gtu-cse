#ifndef GTU_ARRAY_H
#define GTU_ARRAY_H

#include <iostream>
#include <memory>      
#include <new>
#include "Iterable.h"

using std::cerr;
using std::endl;
using std::shared_ptr;
using std::ostream;
using std::out_of_range;
using std::bad_alloc;
using stdGTU::GTUIteratorConst;
using stdGTU::GTUIterator;

namespace stdGTU {
    template<typename T, size_t SIZE>
    class GTUArray : public Iterable<T> {
    public:
        typedef GTUIteratorConst<T> const_iterator;
        typedef GTUIterator<T> iterator;
        
        // cst
        GTUArray ();
        GTUArray (const T & initVal);
        // copy cst
        GTUArray (const GTUArray<T, SIZE> & other);
        // move cst
        GTUArray (GTUArray<T, SIZE> && other);
        // copy=
        GTUArray<T, SIZE> & operator= (const GTUArray<T, SIZE> & other);
        // move=
        GTUArray<T, SIZE> & operator= (GTUArray<T, SIZE> && other);
        // destructor not required

        // return iterator to beginning
        iterator begin () const override; 
        // return iterator to end
        iterator end () const override; 
        // return a constant iterator to beginning
        const_iterator cbegin () const override; 
        // return a constant iterator to end
        const_iterator cend () const override; 

        // return array size (size is always equal to size GTUArray<T, SIZE>)
        size_t size () const override;
        // rrase element pointed by the given iterator
        void erase (iterator it) override;
        // test whether array is empty
        bool empty () const override;
        
        // access element
        T & operator[] (size_t position) ;
        const T & operator[] (size_t position) const;
        T & at (size_t position);
        const T & at (size_t position) const;

        // sets val as the value for all the elements in the array object.
        void fill (const T & val);

        // exchanges the content of the array by the content of other which
        // is another array object of the same type (including the same size).
        void swap (GTUArray & other);

        // clear all content   
        void clear () override;  
        
        // prints the array content
        void print (ostream & os) const override;

    private:
        void checkPosition (size_t position) const;
        shared_ptr<T[]> _ptr;
    };
}

#include "../src/GTUArray.cpp"

#endif
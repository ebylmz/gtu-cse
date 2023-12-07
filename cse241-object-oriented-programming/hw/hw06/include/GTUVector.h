#ifndef GTU_VECTOR_H
#define GTU_VECTOR_H

#include <iostream>
#include <memory>      
#include <new>
#include "Iterable.h"

using std::cerr;
using std::endl;
using std::shared_ptr;
using std::out_of_range;
using std::bad_alloc;
using stdGTU::GTUIteratorConst;
using stdGTU::GTUIterator;

namespace stdGTU {
    template<typename T>
    class GTUVector : public Iterable<T> {
    public: 
        typedef GTUIteratorConst<T> const_iterator;
        typedef GTUIterator<T> iterator;

        // cst
        GTUVector (size_t size = 0);
        GTUVector (size_t size, T initValue);
        // copy cst
        GTUVector (const GTUVector<T> & other);
        // move cst
        GTUVector (GTUVector<T> && other);
        // copy=
        GTUVector<T> & operator= (const GTUVector<T> & rhs);
        // move=
        GTUVector<T> & operator= (GTUVector<T> && rhs);
        // destructor not required

        // return iterator to beginning
        iterator begin () const override; 
        // return iterator to end
        iterator end () const override; 
        // return a constant iterator to beginning
        const_iterator cbegin () const override; 
        // return a constant iterator to end
        const_iterator cend () const override; 
        
        // test whether container is empty
        bool empty () const override;
        // return container size
        size_t size () const override;
        // erase element pointed by the given iterator
        void erase (iterator it) override;
        // return current maximum number of elements the vector can hold   
        size_t capacity () const ;
        // Clear all content   
        void clear () override;        


        //* Element Access
        T & operator[] (size_t position);
        const T & operator[] (size_t position) const;

        T & at (size_t position);
        const T & at (size_t position) const;

        bool operator== (const GTUVector<T> & other) const;
        bool operator!= (const GTUVector<T> & other) const;

        //* Modifiers
        void insert (size_t position, const T & element);
        void insert (size_t position, T && element);

        void pushBack (const T & element);
        void pushBack (T && element);

        void popBack () ;

        // sets size of the vector and assigns given val
        void resize (size_t newsize, const T & val = 0);
        // sets capacity of the vector
        void reserve (size_t newcapacity);   
        // sets container capacity to the size
        void shrinkToFit () ;
    private:
        void checkPosition (size_t position) const;
        
        size_t _size;
        size_t _capacity;
        shared_ptr<T[]> _ptr; // allocated memory
    }; 
}

#include "../src/GTUVector.cpp"

#endif
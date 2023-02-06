#ifndef PFARRAY_H
#define PFARRAY_H

#include <iostream>
#include <new>

using std::bad_alloc;
using std::out_of_range;

namespace stdEBY {
    template<class T>
    class PFArray {
    public:
        PFArray (int capacity = 64);

        // BIG FIVE
        // copy constructor
        PFArray (const PFArray & other);  
        // move constructor
        PFArray (PFArray && other);  
        // copy=
        PFArray<T> & operator= (const PFArray<T> & other) throw(bad_alloc);
        // move=
        PFArray<T> & operator= (PFArray<T> && other) noexcept;
        // destructor   
        virtual ~PFArray ();    
        //! Destructures should always be virtual
        //! otherwise late binding does not happen when operator delete executes
        // https://stackoverflow.com/questions/461203/when-to-use-virtual-destructors

        bool full () const noexcept;
        bool empty () const noexcept;
        int used () const noexcept;
        int capacity () const noexcept;

        // adds given element to the array if the used memory less than capacity
        void add (const T & e) throw (out_of_range);
        void add (T && e) throw (out_of_range);

        // delete all the array content
        void destroy () noexcept;
        // change the capacity of the array
        void reserve (int newCapacity) throw (bad_alloc);

        T & operator[] (int i) throw (out_of_range);
        const T & operator[] (int i) const throw (out_of_range);

    private:
        int _capacity;
        int _used;        
        T * _arr;
    };
}

#endif
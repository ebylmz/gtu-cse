#ifndef ITERABLE_H
#define ITERABLE_H

#include <iostream>
#include "GTUIterator.h"

using std::ostream;
using std::cout;
using std::endl;
using stdGTU::GTUIteratorConst;
using stdGTU::GTUIterator;

namespace stdGTU {
    template<typename T>
    class Iterable {
    public:
        typedef GTUIterator<T> iterator;
        typedef GTUIteratorConst<T> const_iterator;

        // Test whether container is empty
        virtual bool empty () const = 0;
        // Return container size
        virtual size_t size () const = 0;
        // Erase element pointed by the given iterator
        virtual void erase (iterator it) = 0;
        // Clear all content   
        virtual void clear () = 0;        

        // prints the container to the given stream
        virtual void print (ostream & os) const;
        
        // Return iterator to beginning
        virtual iterator begin () const = 0; 
        // Return iterator to end
        virtual iterator end () const = 0; 
        // Return a constant iterator to beginning
        virtual const_iterator cbegin () const = 0; 
        // Return a constant iterator to end
        virtual const_iterator cend () const = 0; 

        // returns next iterator after current iterator
        virtual GTUIterator<T> next (iterator it) const;
        // returns previos iterator after current iterator
        virtual GTUIterator<T> prev (iterator it) const;

    protected:
        // cheks if given iterator is iterator of this container
        bool myIter (iterator it) const;
    };

    template<typename T>
    ostream & operator<< (ostream & os, const Iterable<T> & obj);
}

#include "../src/Iterable.cpp"

#endif 
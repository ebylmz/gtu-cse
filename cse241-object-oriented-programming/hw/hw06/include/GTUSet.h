#ifndef GTU_SET_H
#define GTU_SET_H

#include <iostream>
#include <memory>
#include "Iterable.h"

using std::shared_ptr;
using std::bad_alloc;
using std::out_of_range;
using stdGTU::GTUIteratorConst;
using stdGTU::GTUIterator;
using std::cout;
using std::cerr;
using std::endl;

namespace stdGTU {
    template<typename T>
    class GTUSet : public Iterable<T> {
    public:
        typedef GTUIteratorConst<T> const_iterator;
        typedef GTUIterator<T> iterator;

        // cst
        GTUSet (size_t capacity = 0);  
        // copy cst
        GTUSet (const GTUSet<T> & other);
        // move cst
        GTUSet (GTUSet<T> && other);
        // copy=
        GTUSet<T> & operator= (const GTUSet<T> & other);
        // move=
        GTUSet<T> & operator= (GTUSet<T> && other);
        // destructor not required

        bool operator== (const GTUSet<T> & other);
        bool operator!= (const GTUSet<T> & other);

        // return iterator to beginning
        iterator begin () const override; 
        // return iterator to end
        iterator end () const override; 
        // return a constant iterator to beginning
        const_iterator cbegin () const override; 
        // return a constant iterator to end
        const_iterator cend () const override; 

        // rest whether container is empty
        bool empty () const override;
        // return container size
        size_t size () const override;
        // Removes from the set container either a single element 
        // or a range of elements ([first,last)).
        void erase (const T & element);
        void erase (iterator it) override;
        // checks if given element is inside of the set
        bool inside (const T & element) const;

        void add (const T & element);
        void add (T && element);
                
        const T & operator[] (int position);
        const T & at (int position);

        // returns the union set
        GTUSet<T> operator+ (const GTUSet<T> & other);
        // returns the difference set
        GTUSet<T> operator- (const GTUSet<T> & other);
        // returns the intersection set
        GTUSet<T> operator^ (const GTUSet<T> & other);

        // Exchanges the content of the container by the content of x, 
        // which is another set of the same type. Sizes may differ.
        void swap (GTUSet<T> & other);
        
        void clear () override;

        // prints the set content
        void print (ostream & os) const override;

        // searches the container for an element equivalent
        // to element and returns an iterator to it if found,
        // otherwise it returns an iterator to GTUset::end.
        iterator find (const T & element) const;
        
    private:
        void checkPosition (size_t position) const;
        // sets capacity of the vector
        void reserve ();    

        size_t _size;
        size_t _capacity;
        shared_ptr<T[]> _ptr; // allocated memory
    };
}

#include "../src/GTUSet.cpp"

#endif
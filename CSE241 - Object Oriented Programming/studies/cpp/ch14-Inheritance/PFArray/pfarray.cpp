#include <iostream>
#include <new>
#include <exception>
#include "pfarray.h"

using std::cerr;
using std::endl;
using std::out_of_range;

namespace stdEBY {
    template<class T>
    PFArray<T>::PFArray (int capacity) 
    : _capacity(capacity), _used(0), _arr(new T[capacity]) {} 

    template<class T>
    PFArray<T>::PFArray (const PFArray & other)
    : _capacity(other.capacity()), _used(other.used()), _arr(new T[other.capacity()]) {
        for (int i = 0; i < _used; ++i)
            _arr[i] = other[i];
    }

    template<class T>
    PFArray<T>::PFArray (PFArray && other) 
    : _capacity(other.capacity()), _used(other.used()), _arr(other._arr) {
        // Steal other's arr pointer and make it destructable state
        // because ofter this execution other tmp object gone 
        other._arr(nullptr);
    }
    
    template<class T>
    PFArray<T> & PFArray<T>::operator= (const PFArray<T> & other) throw(bad_alloc) {
        //! no need to overload operator== and operator!=
        if (*this != other) {   
            delete[] _arr;
            _used = other._used;
            _capacity = other._capacity;
            
            _arr = new T[_capacity];
            for (int i = 0; i < other.used(); ++i)
                _arr[i] = other[i];   // operator[] is overloaded
        }
        return *this;
    }
    
    template<class T>
    PFArray<T> & PFArray<T>::operator= (PFArray<T> && other) noexcept {
        delete[] _arr;
        _arr = other._arr;      // steal others array
        other._arr = nullptr;   // make it destructable state
        _used = other._used;
        _capacity = other._capacity;
        return *this;
    }

    template<class T>
    PFArray<T>::~PFArray () {
        if (_arr != nullptr)
            delete[] _arr;
    }

    template<class T>
    bool PFArray<T>::full () const noexcept {return used() == capacity();}
    
    template<class T>
    bool PFArray<T>::empty () const noexcept {return used() == 0;}
    
    template<class T>
    int PFArray<T>::used () const noexcept {return _used;}
    
    template<class T>
    int PFArray<T>::capacity () const noexcept {return _capacity;}

    template<class T>
    void PFArray<T>::add (const T & e) throw (out_of_range) {
        if (used() >= capacity())
            throw out_of_range("out of range");

        _arr[_used] = e;
        ++_used; 
    }
 
    template<class T>
    void PFArray<T>::add (T && e) throw (out_of_range) {
        cout << "NOT IMPLEMENTED YET\n";
    }

    template<class T>
    void PFArray<T>::destroy () noexcept {
        delete[] _arr;
        _arr = NULL;
        _used = 0;
        _capacity = 0;
    }

    template<class T>
    void PFArray<T>::reserve (int newCapacity) throw (bad_alloc) {
        try {
            decltype(_arr) tmp = new T[newCapacity];
            
            int bound = used() < newCapacity ? used() : newCapacity;
            for (int i = 0; i < bound; ++i)
                tmp[i] = _arr[i];
            
            delete[] _arr;
            _arr = tmp;
            _capacity = newCapacity;
        }
        catch (bad_alloc & e) {
            cerr << e.what() << endl;
        }
    }

    template<class T>
    T & PFArray<T>::operator[] (int i) throw (out_of_range) {
        if (i >= used())
            throw out_of_range("out of range");

        return _arr[i];        
    }
    
    template<class T>
    const T & PFArray<T>::operator[] (int i) const throw (out_of_range) {
        if (i >= used())
            throw out_of_range("out of range");

        return _arr[i];        
    }
}

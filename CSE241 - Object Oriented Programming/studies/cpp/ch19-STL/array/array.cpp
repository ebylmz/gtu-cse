#include "array.h"

namespace stdEBY {
    template<class T, size_t N>
    Array<T, N>::Array () 
    : _arr(new T[N]) {}

    template<class T, size_t N>
    Array<T, N>::~Array () {
        delete[] _arr;
    }

    template<class T, size_t N> 
    int Array<T, N>::size () const {return N;}

    template<class T, size_t N> 
    bool Array<T, N>::empty () const {return N == 0;}


    template<class T, size_t N> 
    T & Array<T, N>:: operator[] (int i) noexcept {
        return _arr[i];
    }

    template<class T, size_t N>    
    const T & Array<T, N>:: operator[] (int i) const noexcept{
        return _arr[i];
    }

    template<class T, size_t N> 
    T & Array<T, N>::at (int i) throw(out_of_range) {
        if (0 <= i && i < N)
            return _arr[i];
        throw out_of_range("Invalid attempt to reach out of range index");
    }

    template<class T, size_t N> 
    const T & Array<T, N>::at (int i) const throw(out_of_range) {
        if (0 <= i && i < N)
            return _arr[i];
        throw out_of_range("Invalid attempt to reach out of range index");
    }

    template<class T, size_t N> 
    void Array<T, N>::fill (const T & value) {
        for (int i = 0; i < N; ++i)
            _arr[i] = value;
    }

    template<class T, size_t N> 
    void Array<T, N>::fill (T && value) {
        for (int i = 0; i < N; ++i)
            _arr[i] = value;    //!!!!!!!!!!!!!
    }
}
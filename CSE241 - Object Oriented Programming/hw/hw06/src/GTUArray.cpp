namespace stdGTU {
    template<typename T, size_t SIZE>
    GTUArray<T, SIZE>::GTUArray () : _ptr(new T[SIZE]) {}
    
    template<typename T, size_t SIZE>
    GTUArray<T, SIZE>::GTUArray (const T & initVal) : GTUArray() {
        for (int i = 0; i < size(); ++i)
            _ptr[i] = initVal;
    }

    template<typename T, size_t SIZE>
    GTUArray<T, SIZE>::GTUArray (const GTUArray<T, SIZE> & other) 
    : _ptr(new T[SIZE]) {
        *this = other;  // copies all other element content
    }

    template<typename T, size_t SIZE>
    GTUArray<T, SIZE>::GTUArray (GTUArray<T, SIZE> && other)
    : _ptr(other._ptr) {
        // reset the shr_pointer to make other in a destructable state
        other._ptr.reset();
    }

    template<typename T, size_t SIZE>
    GTUArray<T, SIZE> & GTUArray<T, SIZE>::operator= (const GTUArray<T, SIZE> & other) {
        // no need to check size since function cannot call 
        // without providing proper arguments which type GTUArray<T, SIZE>
        for (int i = 0; i < SIZE; ++i)
            _ptr[i] = other._ptr[i];
    }

    template<typename T, size_t SIZE>
    GTUArray<T, SIZE> & GTUArray<T, SIZE>::operator= (GTUArray<T, SIZE> && other) {
        for (int i = 0; i < SIZE; ++i)
            _ptr[i] = std::move(other._ptr[i]);
    }

    template<typename T, size_t SIZE>
    typename GTUArray<T, SIZE>::iterator GTUArray<T, SIZE>::begin () const {
        return iterator(_ptr.get());
    } 

    template<typename T, size_t SIZE>
    typename GTUArray<T, SIZE>::iterator GTUArray<T, SIZE>::end () const {
        return iterator(_ptr.get() + size());
    }

    template<typename T, size_t SIZE>
    typename GTUArray<T, SIZE>::const_iterator GTUArray<T, SIZE>::cbegin () const {
        return const_iterator(_ptr.get());
    }

    template<typename T, size_t SIZE>
    typename GTUArray<T, SIZE>::const_iterator GTUArray<T, SIZE>::cend () const {
        return const_iterator(_ptr.get() + size());
    }

    template<typename T, size_t SIZE>
    size_t GTUArray<T, SIZE>::size () const {return SIZE;}

    template<typename T, size_t SIZE>
    void GTUArray<T, SIZE>::erase (iterator it) {
        if (size() > 0 && this->myIter(it))
            *it = T();  // assign anonymous object
    }

    template<typename T, size_t SIZE>
    bool GTUArray<T, SIZE>::empty () const {return SIZE == 0;}

    template<typename T, size_t SIZE>
    T & GTUArray<T, SIZE>::operator[] (size_t position) {return _ptr[position];}

    template<typename T, size_t SIZE>
    const T & GTUArray<T, SIZE>::operator[] (size_t position) const {return _ptr[position];}

    template<typename T, size_t SIZE>
    T & GTUArray<T, SIZE>::at (size_t position) {
        try {
            checkPosition(position);
            return _ptr[position];
        }
        catch (out_of_range & e) {
            cerr << e.what() << " Aborted." << endl;
            exit(1);
        }
    }

    template<typename T, size_t SIZE>
    const T & GTUArray<T, SIZE>::at (size_t position) const {
        try {
            checkPosition(position);
            return _ptr[position];
        }
        catch (out_of_range & e) {
            cerr << e.what() << " Aborted." << endl;
            exit(1);
        }    
    }

    template<typename T, size_t SIZE>
    void GTUArray<T, SIZE>::fill (const T & val)  {
        for (int i = 0; i < size(); ++i)
            _ptr[i] = val;
    }

    template<typename T, size_t SIZE>
    void GTUArray<T, SIZE>::swap (GTUArray & other)  {
        if (size() == other.size()) {
            // swap the pointers which keep the data
            auto tmp = _ptr;
            _ptr = other._ptr;
            other._ptr = tmp;
        }
    }

    template<typename T, size_t SIZE>
    void GTUArray<T, SIZE>::clear ()  {
        for (int i = 0; i < size(); ++i)
            _ptr[i] = T();  // anonymus object    
    }  

    template<typename T, size_t SIZE>
    void GTUArray<T, SIZE>::print (ostream & os) const {
        os << "[";
        for (auto it = cbegin(); it != cend(); ++it) {
            if (it != cbegin())
                os << ", ";
            os << *it;
        }
        os << "]";
    }

    template<typename T, size_t SIZE>
    void GTUArray<T, SIZE>::checkPosition (size_t position) const {
        if (position < 0 || position >= SIZE)
            throw out_of_range("Attempt to invalid array position.");
    }
}
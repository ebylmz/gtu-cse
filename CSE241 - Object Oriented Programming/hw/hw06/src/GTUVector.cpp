namespace stdGTU {
    template<typename T>
    GTUVector<T>::GTUVector (size_t size) 
    : _size(size < 0 ? 0 : size), _capacity(_size), _ptr(new T[_capacity]) {}

    template<typename T>
    GTUVector<T>::GTUVector (size_t size, T initValue)
    : GTUVector(size) {
        for (int i = 0; i < _capacity; ++i)
            _ptr[i] = initValue;
    }

    template<typename T>
    GTUVector<T>::GTUVector (const GTUVector<T> & other)
    : _size(other.size()), _capacity(other.capacity()), _ptr(new T[_capacity]) {
        // copy all the data
        for (int i = 0; i < _size; ++i)
            _ptr[i] = other._ptr[i];
    }

    template<typename T>
    GTUVector<T>::GTUVector (GTUVector<T> && other)
    : _size(other.size()), _capacity(other.capacity(), _ptr(other._ptr)) {
        // reset the shr_pointer to make other in a destructable state
        other._ptr.reset();
    }

    template<typename T>
    GTUVector<T> & GTUVector<T>::operator= (const GTUVector<T> & rhs) {
        if (*this != rhs) {
            // allocate new memory if needed
            if (capacity() != rhs.capacity()) {
                try {
                    // tmp holds new allocated memory
                    shared_ptr<T[]> tmp(new T[rhs.capacity()]);
                    _size = rhs.size();
                    _capacity = rhs.capacity();
                    
                    // copy all the data to the new allocated memory
                    for (int i = 0; i < _size; ++i)
                        tmp[i] = rhs._ptr[i];
                    
                    _ptr = tmp;     // pre allocated memory is free
                }
                catch (bad_alloc) {
                    cerr << "Run out of memory. Aborted\n";
                    exit(1);  
                }  
            }
        }
        return *this;
    }

    template<typename T>
    GTUVector<T> & GTUVector<T>::operator= (GTUVector<T> && rhs) {
        _capacity = rhs.capacity();
        _size = rhs.size();
        _ptr = rhs._ptr;        // steal other's data address
        rhs._ptr = nullptr;     // make rhs in a destructable state
        return *this;
    }

    template<typename T>
    typename GTUVector<T>::iterator GTUVector<T>::begin () const {
        return iterator(_ptr.get());
    } 

    template<typename T>
    typename GTUVector<T>::iterator GTUVector<T>::end () const {
        return iterator(_ptr.get() + size());
    }

    template<typename T>
    typename GTUVector<T>::const_iterator GTUVector<T>::cbegin () const {
        return const_iterator(_ptr.get());
    }

    template<typename T>
    typename GTUVector<T>::const_iterator GTUVector<T>::cend () const {
        return const_iterator(_ptr.get() + size());
    }   

    template<typename T>
    bool GTUVector<T>::empty () const {return size() == 0;}

    template<typename T>
    size_t GTUVector<T>::size () const {return _size;}
    
    template<typename T>
    void GTUVector<T>::erase (iterator it) {
        // slide the elements left and erase the last element by decreasing size 
        if (size() > 0 && this->myIter(it)) {
            for (auto next_it = this->next(it); next_it != end(); ++it, ++next_it)
                *it = std::move(*next_it);
            --_size;
        }
    }

    template<typename T>
    size_t GTUVector<T>::capacity () const {return _capacity;}

    template<typename T>
    void GTUVector<T>::clear ()  {
        _size = 0;
    }        

    template<typename T>
    T & GTUVector<T>::operator[] (size_t position)  {
        return _ptr[position];
    }

    template<typename T>
    const T & GTUVector<T>::operator[] (size_t position) const  {
        return _ptr[position];
    }

    template<typename T>
    T & GTUVector<T>::at (size_t position) {
        try {
            checkPosition(position);
            return _ptr[position];
        }
        catch (out_of_range & e) {
            cerr << e.what() << " Aborted." << endl;
            exit(1);
        }
    }

    template<typename T>
    const T & GTUVector<T>::at (size_t position) const {
        try {
            checkPosition(position);
            return _ptr[position];
        }
        catch (out_of_range & e) {
            cerr << e.what() << " Aborted." << endl;
            exit(1);
        }
    }

    template<typename T>
    bool GTUVector<T>::operator== (const GTUVector & other) const  {
        if (size() == other.size() && capacity() == other.capacity()) {
            for (int i = 0; i < size(); ++i)
                if (_ptr[i] != other._ptr[i])
                    return false;
            return true;
        }
        return false;
    }

    template<typename T>
    bool GTUVector<T>::operator!= (const GTUVector & other) const  {
        return !(*this == other);
    }
    
    template<typename T>
    void GTUVector<T>::insert (size_t position, const T & element) {
        pushBack(std::move(element)); 

        // swap all the elements left till reach the position
        for (int i = size() - 1; i > position; --i)
            _ptr[i] = _ptr[i - 1];
        _ptr[position] = element;
    }
    
    template<typename T>
    void GTUVector<T>::insert (size_t position, T && element) {
        pushBack(element); 

        // swap all the elements left till reach the position
        for (int i = size() - 1; i > position; --i)
            _ptr[i] = _ptr[i - 1];
        _ptr[position] = std::move(element);
    }

    template<typename T>
    void GTUVector<T>::pushBack (const T & element)  {
        if (_ptr == nullptr || size() == capacity())
            reserve(capacity() == 0 ? 16 : capacity() * 2);

        _ptr[_size] = element;
        ++_size;
    }

    template<typename T>
    void GTUVector<T>::pushBack (T && element)  {
        if (_ptr == nullptr || size() == capacity())
            reserve(capacity() == 0 ? 16 : capacity() * 2);

        _ptr[_size] = std::move(element);
        ++_size;
    }

    template<typename T>
    void GTUVector<T>::popBack ()  {
        if (size() > 0)
            --_size;
    }
    
    template<typename T>
    void GTUVector<T>::resize (size_t newsize, const T & val) {
        // reserve memory if newsize larger than current capacity
        if (newsize > capacity())
            reserve(newsize);
        
        _size = newsize;
        for (int i; i < _size; ++i)
            _ptr[i] = val;
    }

    template<typename T>
    void GTUVector<T>::reserve (size_t newcapacity) {
        if (newcapacity >= 0) {
            try {
                // allocate new memory and copy all elements to new allocated memory 
                shared_ptr<T[]> tmp(new T[newcapacity]);

                // in case of newcapacity smaller than current capacity
                // be sure size does not exceeds newcapacity
                _capacity = newcapacity;
                if (_capacity < size())
                    _size = _capacity;

                if (_ptr != NULL)
                    for (int i = 0; i < _size; ++i)
                        tmp[i] = std::move(_ptr[i]);
                
                _ptr = tmp;  // shared_ptr automaticly frees it's pre allocated memory
            }
            catch (bad_alloc) {
                cerr << "Run out of memory. Aborted\n";
                exit(1);  
            }
        }
    } 
    
    template<typename T>
    void GTUVector<T>::shrinkToFit ()  {
        reserve(size());
    }
    
    template<typename T>
    void GTUVector<T>::checkPosition (size_t position) const {
        if (position < 0 || position >= size())
            throw out_of_range("Attempt to invalid vector position.");
    }
}
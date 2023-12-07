namespace {
    template<typename T> 
    void swap (T & x, T & y) {
        T tmp = std::move(x);
        x = std::move(y);
        y = std::move(tmp);
    } 
}

namespace stdGTU {
    template<typename T>
    GTUSet<T>::GTUSet (size_t capacity) 
    : _size(0), _capacity(capacity <= 0 ? 0 : capacity), _ptr(new T[_capacity])
    {}   

    template<typename T>
    GTUSet<T>::GTUSet (const GTUSet<T> & other)
    : _size(other.size()), _capacity(other._capacity), _ptr(new T[_capacity]) {
        for (int i = 0; i < _size; ++i)
            _ptr[i] = other._ptr[i];
    }

    template<typename T>
    GTUSet<T>::GTUSet (GTUSet<T> && other)
    : _size(other.size()), _capacity(other._capacity), _ptr(other._ptr) {
        // reset the shr_pointer to make other in a destructable state
        other._ptr.reset();
    }

    template<typename T>
    GTUSet<T> & GTUSet<T>::operator= (const GTUSet<T> & other) {
        // if they are not same
        if (_ptr != other._ptr) {
            // set size and the capacity
            if (_capacity != other._capacity) {
                _capacity = other._capacity;
                _ptr = new T[_capacity]; 
            }
            _size = other.size();
            
            for (int i = 0; i < size(); ++i)
                _ptr[i] = other._ptr[i];
        }
        return *this;
    }

    template<typename T>
    GTUSet<T> & GTUSet<T>::operator= (GTUSet<T> && other) {
        _size = other.size(); 
        _capacity = other._capacity;
        _ptr = other._ptr;
        other._ptr = nullptr; 
        return *this;
    }

    template<typename T>
    bool GTUSet<T>::operator== (const GTUSet<T> & other) {
        if (size() == other.size()) {
            for (auto it = begin(); it != end(); ++it)
                if (!other.inside(*it))
                    return false;
            return true;
        }
        return false;
    }

    template<typename T>
    bool GTUSet<T>::operator!= (const GTUSet<T> & other) {return !(*this == other);}

    template<typename T>
    typename GTUSet<T>::iterator GTUSet<T>::begin () const {
        return iterator(_ptr.get());
    } 

    template<typename T>
    typename GTUSet<T>::iterator GTUSet<T>::end () const {
        return iterator(_ptr.get() + size());
    }

    template<typename T>
    typename GTUSet<T>::const_iterator GTUSet<T>::cbegin () const {
        return const_iterator(_ptr.get());
    }

    template<typename T>
    typename GTUSet<T>::const_iterator GTUSet<T>::cend () const {
        return const_iterator(_ptr.get() + size());
    }   

    template<typename T>
    bool GTUSet<T>::empty () const {return size() == 0;}

    template<typename T>
    size_t GTUSet<T>::size () const {return _size;}

    template<typename T> 
    void GTUSet<T>::erase (const T & element) {
        erase(find(element));
    }

    template<typename T> 
    void GTUSet<T>::erase (iterator it) {
        if (size() > 0 && this->myIter(it)) {
            for (auto next_it = this->next(it); next_it != end(); ++it, ++next_it)
                *it = std::move(*next_it);
            --_size;
        }
    }
    
    template<typename T>
    bool GTUSet<T>::inside (const T & element) const {
        // If the container is empty, begin() and end() 
        // returns iterators whose value will be nullptr
        // return begin() != nullptr && find(element) != end();
        return size() != 0 && find(element) != end();
    }

    template<typename T> 
    void GTUSet<T>::add (const T & element) {
        // if given element does not exist in set
        if (!inside(element)) {
            // reallocate memory for the set if needed
            if (size() == _capacity)
                reserve();

            _ptr[_size] = element;
            ++_size; 
        }
    }

    template<typename T> 
    void GTUSet<T>::add (T && element) {
        if (!inside(element)) {
            // reallocate memory for the set if needed
            if (size() == _capacity)
                reserve();

            _ptr[_size] = std::move(element);   
            ++_size; 
        }
    }

    template<typename T> 
    const T & GTUSet<T>::operator[] (int position) {
        return _ptr[position];
    }
    
    template<typename T> 
    const T & GTUSet<T>::at (int position) {
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
    GTUSet<T> GTUSet<T>::operator+ (const GTUSet<T> & other) {
        GTUSet<T> newset(other);
        for (auto it = begin(); it != end(); ++it)
            newset.add(*it);
        return std::move(newset);
    }

    template<typename T> 
    GTUSet<T> GTUSet<T>::operator- (const GTUSet<T> & other) {
        GTUSet<T> newset;
        // add the elements which are belongs to only this set
        for (auto it = begin(); it != end(); ++it)
            if (!other.inside(*it))
                newset.add(*it);
        return std::move(newset);
    }

    template<typename T> 
    GTUSet<T> GTUSet<T>::operator^ (const GTUSet<T> & other) {
        GTUSet<T> newset;
        // add the elements which are belongs to both this and other set
        for (auto it = begin(); it != end(); ++it)
            if (other.inside(*it))
                newset.add(*it);
        return std::move(newset);
    }

    template<typename T> 
    void GTUSet<T>::swap (GTUSet<T> & other) {
        if (*this != other) {
            // just swap the pointers and other informations
            swap(_size, other._size);
            swap(_capacity, other._capacity);
            swap(_ptr, other._ptr);
        }
    }

    template<typename T>
    void GTUSet<T>::clear () {_size = 0;}

    template<typename T>
    void GTUSet<T>::print (ostream & os) const  {
        os << "{";
        for (auto it = cbegin(); it != cend(); ++it) {
            if (it != cbegin())
                os << ", ";
            os << *it;
        }
        os << "} " << "(size: " << size() << ")";    
    }


    template<typename T>
    typename GTUSet<T>::iterator GTUSet<T>::find (const T & element) const {
        GTUSet<T>::iterator it = begin();
        while (it != end() && *it != element) 
            ++it;
        return it;
    }

    template<typename T>
    void GTUSet<T>::reserve () {
        _capacity = (_capacity == 0) ? 1 : _capacity * 2;

        try {
            shared_ptr<T[]> tmp(new T[_capacity]);
            for (int i = 0; i < size(); ++i)
                tmp[i] = std::move(_ptr[i]);
            _ptr = tmp;
        }
        catch (bad_alloc) {
            cerr << "Run out of memory. Aborted\n";
            exit(1);  
        }
    }

    template<typename T>
    void GTUSet<T>::checkPosition (size_t position) const {
        if (position < 0 || position >= size())
            throw out_of_range("Attempt to invalid set position.");
    }
}
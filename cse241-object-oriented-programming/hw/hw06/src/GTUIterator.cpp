namespace stdGTU
{
    /* =========================================================================== */
    /* ============================ GTUIteratorConst ============================= */
    /* =========================================================================== */

    template<typename DataType>
    GTUIteratorConst<DataType>::GTUIteratorConst(DataType *ptr) : _ptr(ptr) {}

    template<typename DataType>
    GTUIteratorConst<DataType> & GTUIteratorConst<DataType>::operator= (DataType * ptr) {
        _ptr = ptr;
        return (*this);
    }

    template<typename DataType>
    GTUIteratorConst<DataType>::operator bool() const {return getConstPtr() != nullptr;}

    template<typename DataType>
    bool GTUIteratorConst<DataType>::operator== (const GTUIteratorConst<DataType> & other) const {return (getConstPtr() == other.getConstPtr());}

    template<typename DataType>
    bool GTUIteratorConst<DataType>::operator!=(const GTUIteratorConst<DataType> & other) const {return (getConstPtr() != other.getConstPtr());}

    template<typename DataType>
    GTUIteratorConst<DataType> & GTUIteratorConst<DataType>::operator+=(const difference_type & movement) {
        _ptr += movement;
        return (*this);
    }

    template<typename DataType>
    GTUIteratorConst<DataType> & GTUIteratorConst<DataType>::operator-= (const difference_type & movement) {
        _ptr -= movement;
        return (*this);
    }

    template<typename DataType>
    GTUIteratorConst<DataType> & GTUIteratorConst<DataType>::operator++ () {
        ++_ptr;
        return (*this);
    }

    template<typename DataType>
    GTUIteratorConst<DataType> & GTUIteratorConst<DataType>::operator-- () {
        --_ptr;
        return (*this);
    }

    template<typename DataType>
    GTUIteratorConst<DataType> GTUIteratorConst<DataType>::operator++ (int) {
        auto temp(*this);
        ++_ptr;
        return temp;
    }

    template<typename DataType>
    GTUIteratorConst<DataType> GTUIteratorConst<DataType>::operator-- (int) {
        auto temp(*this);
        --_ptr;
        return temp;
    }

    template<typename DataType>
    GTUIteratorConst<DataType> GTUIteratorConst<DataType>::operator+ (const difference_type & movement) {
        return GTUIteratorConst<DataType>(getConstPtr() + movement);
    }

    template<typename DataType>
    GTUIteratorConst<DataType> GTUIteratorConst<DataType>::operator- (const difference_type & movement) { 
        return GTUIteratorConst<DataType>(getConstPtr() - movement); 
    }

    template<typename DataType>
    std::ptrdiff_t GTUIteratorConst<DataType>::operator- (const GTUIteratorConst<DataType> & other) { 
        return std::distance(other.getConstPtr(), getConstPtr()); 
    }

    template<typename DataType>
    const DataType & GTUIteratorConst<DataType>::operator* () const {return *getConstPtr();}

    template<typename DataType>
    const DataType * GTUIteratorConst<DataType>::operator-> () const {return getConstPtr();}

    template<typename DataType>
    const DataType * GTUIteratorConst<DataType>::getConstPtr () const {return _ptr;}

    /* =========================================================================== */
    /* =============================== GTUIterator =============================== */
    /* =========================================================================== */

    template<typename DataType>
    GTUIterator<DataType>::GTUIterator (DataType * ptr) : GTUIteratorConst<DataType>(ptr) {}

    template<typename DataType>
    GTUIterator<DataType> & GTUIterator<DataType>::operator= (DataType * ptr) {
        this->_ptr = ptr;
        return (*this);
    }

    template<typename DataType>
    GTUIterator<DataType> & GTUIterator<DataType>::operator+=(const difference_type & movement) {
        this->_ptr += movement;
        return (*this);
    }

    template<typename DataType>
    GTUIterator<DataType> & GTUIterator<DataType>::operator-= (const difference_type & movement) {
        this->_ptr -= movement;
        return (*this);
    }

    template<typename DataType>
    GTUIterator<DataType> & GTUIterator<DataType>::operator++ () {
        ++(this->_ptr);
        return (*this);
    }

    template<typename DataType>
    GTUIterator<DataType> & GTUIterator<DataType>::operator-- () {
        --(this->_ptr);
        return (*this);
    }

    template<typename DataType>
    GTUIterator<DataType> GTUIterator<DataType>::operator++ (int) {
        auto temp(*this);
        ++(this->_ptr);
        return temp;
    }

    template<typename DataType>
    GTUIterator<DataType> GTUIterator<DataType>::operator-- (int) {
        auto temp(*this);
        --(this->_ptr);
        return temp;
    }

    template<typename DataType>
    GTUIterator<DataType> GTUIterator<DataType>::operator+ (const difference_type & movement) {
        return GTUIterator<DataType>(getPtr() + movement);
    }

    template<typename DataType>
    GTUIterator<DataType> GTUIterator<DataType>::operator- (const difference_type & movement) { 
        return GTUIterator<DataType>(getPtr() - movement); 
    }

    template<typename DataType>
    std::ptrdiff_t GTUIterator<DataType>::operator- (const GTUIterator<DataType> & other) {
        return std::distance(other.getPtr(), getPtr()); 
    }

    template<typename DataType>
    DataType & GTUIterator<DataType>::operator* () {return *getPtr();}

    template<typename DataType>
    DataType * GTUIterator<DataType>::operator-> () {return getPtr();}

    template<typename DataType>
    DataType * GTUIterator<DataType>::getPtr () const {return this->_ptr;}
}
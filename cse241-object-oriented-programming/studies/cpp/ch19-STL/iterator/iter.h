//-------------------------------------------------------------------
// Raw iterator with random access
//-------------------------------------------------------------------
template<typename DataType>
class RawIterator {
public:

    using iterator_category = std::random_access_iterator_tag;
    using value_type = DataType;
    using difference_type = std::ptrdiff_t;
    using pointer = DataType *;
    using reference = DataType &;

public:

    RawIterator (DataType * ptr = nullptr) : _ptr(ptr) {}

    RawIterator<DataType> & operator= (DataType * ptr) {_ptr = ptr; return (*this);}

    operator bool() const {return _ptr != nullptr;}

    bool operator== (const RawIterator<DataType>& rawIterator) const {return (_ptr == rawIterator.getConstPtr());}
    bool operator!= (const RawIterator<DataType>& rawIterator) const {return (_ptr != rawIterator.getConstPtr());}

    RawIterator<DataType> & operator+= (const difference_type& movement) {_ptr += movement; return (*this);}
    RawIterator<DataType> & operator-= (const difference_type& movement) {_ptr -= movement; return (*this);}
    RawIterator<DataType> & operator++ () {++_ptr; return (*this);}
    RawIterator<DataType> & operator-- () {--_ptr; return (*this);}
    RawIterator<DataType> operator++ (int) {auto temp(*this); ++_ptr; return temp;}
    RawIterator<DataType> operator-- (int) {auto temp(*this); --_ptr; return temp;}
    RawIterator<DataType> operator+ (const difference_type& movement) {return RawIterator<DataType>(_ptr + movement);}
    RawIterator<DataType> operator- (const difference_type& movement) {return RawIterator<DataType>(_ptr - movement);}

    difference_type operator-(const RawIterator<DataType>& other){return std::distance(other.getPtr(),this->getPtr());}

    DataType & operator* () {return *_ptr;}
    const DataType & operator* () const {return *_ptr;}
    DataType * operator-> () {return _ptr;}   //! NOT SURE

    DataType * getPtr () const {return _ptr;}
    const DataType * getConstPtr () const {return _ptr;}

protected:

    DataType * _ptr;
};
//-------------------------------------------------------------------

//-------------------------------------------------------------------
// Raw reverse iterator with random access
//-------------------------------------------------------------------
template<typename DataType>
class RawReverseIterator : public RawIterator<DataType> {
public:

    using iterator_category = std::random_access_iterator_tag;
    using value_type = DataType;
    using difference_type = std::ptrdiff_t;
    using pointer = DataType *;
    using reference = DataType &;

public:

    RawReverseIterator (DataType* ptr = nullptr) : RawIterator<DataType>(ptr) {}
    RawReverseIterator (const RawIterator<DataType> & rawIterator) : RawIterator<DataType>(rawIterator.getPtr) {}

    RawReverseIterator<DataType> & operator= (const RawIterator<DataType>& rawIterator){this->_ptr = rawIterator.getPtr();return (*this);}
    RawReverseIterator<DataType> & operator= (DataType * ptr) {this->_ptr = ptr; return (*this);}

    RawReverseIterator<DataType> & operator+= (const difference_type & movement) {this->_ptr -= movement; return (*this);}
    RawReverseIterator<DataType> & operator-= (const difference_type & movement) {this->_ptr += movement; return (*this);}
    RawReverseIterator<DataType> & operator++ () {--this->_ptr; return (*this);}
    RawReverseIterator<DataType> & operator-- () {++this->_ptr; return (*this);}
    RawReverseIterator<DataType> operator++ (int) {auto temp(*this); --this->_ptr; return temp;}
    RawReverseIterator<DataType> operator-- (int) {auto temp(*this); ++this->_ptr; return temp;}
    RawReverseIterator<DataType> operator+ (const int& movement) {return RawReverseIterator(this->_ptr - movement);}
    RawReverseIterator<DataType> operator- (const int& movement) {return RawReverseIterator(this->_ptr + movement);}

    difference_type operator- (const RawReverseIterator<DataType> & other){return std::distance(this->getPtr(), other.getPtr());}

    RawIterator<DataType> base(){RawIterator<DataType> forwardIterator(this->_ptr); ++forwardIterator; return forwardIterator;}
};
//-------------------------------------------------------------------

template<typename T>
class Container {
public: // The typedefs

    typedef RawIterator<T>              iterator;
    typedef RawIterator<const T>        const_iterator;

    typedef RawReverseIterator<T>       reverse_iterator;
    typedef RawReverseIterator<const T> const_reverse_iterator;

public:  

    Container (size_t capacity = 0) 
    : _size(0), _capacity(capacity <= 0 ? 0 : capacity), _mdata(new T[_capacity]) {}

    // The begin/end functions
    iterator begin () {return iterator(_mdata);}
    iterator end () {return iterator(_mdata + _size);}

    const_iterator cbegin () {return const_iterator(_mdata);}
    const_iterator cend () {return const_iterator(_mdata + _size);}

    reverse_iterator rbegin () {return reverse_iterator(_mdata + _size - 1);}
    reverse_iterator rend () {return reverse_iterator(_mdata - 1);}

    const_reverse_iterator crbegin () {return const_reverse_iterator(_mdata + _size - 1);}
    const_reverse_iterator crend () {return const_reverse_iterator(_mdata - 1);}

    size_t size () {return _size;}
    size_t capacity () {return _capacity;}

    T & operator[] (int position) {return _mdata[position];}

    // Utility functions
    void push_back (const T & element) {
        if (size() == capacity()) 
            reserve(capacity() == 0 ? 1 : capacity() * 2);
        _mdata[_size] = element;
        ++_size;
    }

    void pop_back () {if (size() > 0) --_size;}
    
    void reserve (size_t newcapacity) {
        _capacity = (newcapacity <= 0) ? 0 : newcapacity;
        _size = (_capacity < _size) ? _capacity : _size;
        T * tmp = _mdata;
        _mdata = new T[capacity()];

        if (tmp != nullptr) {
            for (int i = 0; i < size(); ++i)
                _mdata[i] = tmp[i];
            delete tmp;
        } 
    }
    
protected:

    size_t _size;
    size_t _capacity;
    T * _mdata;   // data in memory
};
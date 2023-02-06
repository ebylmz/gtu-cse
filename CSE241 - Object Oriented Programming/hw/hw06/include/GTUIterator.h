#ifndef GTU_ITERATOR_H
#define GTU_ITERATOR_H

namespace stdGTU {
template<typename DataType>    
    class GTUIteratorConst {
    public: // iterator tags
        using iterator_category = std::random_access_iterator_tag;
        using value_type = DataType;
        using difference_type = std::ptrdiff_t;
        using pointer = DataType *;
        using reference = DataType &;

    public:
        GTUIteratorConst (DataType * ptr = nullptr);

        GTUIteratorConst<DataType> & operator= (DataType * ptr);

        operator bool () const;
        
        bool operator== (const GTUIteratorConst<DataType> & other) const;
        bool operator!= (const GTUIteratorConst<DataType> & other) const;

        GTUIteratorConst<DataType> & operator+= (const difference_type & movement);
        GTUIteratorConst<DataType> & operator-= (const difference_type & movement);
        GTUIteratorConst<DataType> & operator++ ();
        GTUIteratorConst<DataType> & operator-- ();
        GTUIteratorConst<DataType> operator++ (int);
        GTUIteratorConst<DataType> operator-- (int);
        GTUIteratorConst<DataType> operator+ (const difference_type & movement);
        GTUIteratorConst<DataType> operator- (const difference_type & movement);

        difference_type operator- (const GTUIteratorConst<DataType> & other);

        const DataType & operator* () const;
        const DataType * operator-> () const; 

        const DataType * getConstPtr () const;

    protected:
        DataType * _ptr;    
    };


template<typename DataType>    
    class GTUIterator : public GTUIteratorConst<DataType> {
    public: // iterator tags
        using iterator_category = std::random_access_iterator_tag;
        using value_type = DataType;
        using difference_type = std::ptrdiff_t;
        using pointer = DataType *;
        using reference = DataType &;

    public:
        GTUIterator (DataType * ptr = nullptr);

        GTUIterator<DataType> & operator= (DataType * ptr);

        GTUIterator<DataType> & operator+= (const difference_type & movement);
        GTUIterator<DataType> & operator-= (const difference_type & movement);
        GTUIterator<DataType> & operator++ ();
        GTUIterator<DataType> & operator-- ();
        GTUIterator<DataType> operator++ (int);
        GTUIterator<DataType> operator-- (int);
        GTUIterator<DataType> operator+ (const difference_type & movement);
        GTUIterator<DataType> operator- (const difference_type & movement);

        difference_type operator- (const GTUIterator<DataType> & other);

        DataType & operator* ();
        DataType * operator-> (); 

        DataType * getPtr () const;
    };
}

#include "../src/GTUIterator.cpp"

#endif
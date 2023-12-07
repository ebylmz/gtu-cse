namespace stdGTU {
    template<typename T>
    void Iterable<T>::print (ostream & os) const {
        os << "{";
        for (auto it = cbegin(); it != cend(); ++it) {
            if (it != cbegin())
                os << ", ";
            os << *it;
        }
        os << "}";
    }

    template<typename T>
    ostream & operator<< (ostream & os, const Iterable<T> & obj) {
        obj.print(os);
        return os;
    }

    template<typename T>
    GTUIterator<T> Iterable<T>::next (iterator it) const {
        if (it != end())
            ++it;
        return it;
    }
 
    template<typename T>
    GTUIterator<T> Iterable<T>::prev (iterator it) const {
        if (it != begin())
            --it;
        return it;
    }

    template<typename T>
    bool Iterable<T>::myIter (iterator it) const {
        auto ptr = it.getPtr();
        return ptr != nullptr && begin().getPtr() <= ptr && ptr < end().getPtr();
    }
}
#include <iostream>
#include <string>
#include "metaData.h"

namespace ARR {
    MetaData::MetaData (int size = 0, const string & name) 
    : _size(size), _name(name) {}

    MetaData::MetaData (const MetaData & other)
    : _size(other.size()), _name(other.name()) {}

    MetaData::MetaData (MetaData && other)
    : _size(other._size),
      _name(std::move(other._name)) {

    } 

    string MetaData::name () const {return _name;}

    int MetaData::size () const {return _size;}
}
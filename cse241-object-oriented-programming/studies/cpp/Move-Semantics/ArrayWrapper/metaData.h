#ifndef META_DATA_H
#define META_DATA_H

#include <iostream>
#include <string>

using std::string;

namespace ARR {
    class MetaData {
    public:
        MetaData (int size, const string & name);
        
        // copy constructor
        MetaData (const MetaData & other); 

        // move constructor
        MetaData (MetaData && other); 

        string name () const;
        int size () const;
    private:
        string _name;
        int _size;
    };
}

#endif
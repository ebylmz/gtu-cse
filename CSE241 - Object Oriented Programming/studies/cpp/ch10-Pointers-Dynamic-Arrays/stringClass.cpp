// page 455

class MyString {
public:
    MyString (int capacity);
    MyString (const MyString & other);  // Copy constructor

    // Destructor (only one)
    ~MyString ();   


    int getCapacity () const;
    int getUsed () const;

    const MyString & operator= (const MyString & rhside);
    // operator= must be a member function

    char operator[] (int index) const;
private:
    char * s;
    int capacity;
    int used;
};

const MyString & MyString::operator= (const MyString & rhside) {
    // It ensures that if the same object occurs on both sides of
    // the assignment operator, then the array named by the member variable a will not be
    // deleted with a call to delete .
    // myString = myString;
    if (getCapacity() != rhside.getCapacity()) {
        delete [] s;
        capacity = rhside.getCapacity();
        s = new char[getCapacity()];
    }
    used = rhside.getUsed();

    for (int i = 0; i < rhside.getUsed(); ++i)
        s[i] = rhside.s[i];
    // '\0' ???
    return *this;
}

// The copy constructor, the = assignment operator, and the destructor are called
// the big three because experts say that if you need any of them, you need all three.
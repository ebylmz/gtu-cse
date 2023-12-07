* operator[] is overload as const and nonconst 
The non-const function will always be called on a non-const array, and the const function on a const array.

When you have two methods with the same name, the compiler selects the best-fitting one based on the type of the arguments, and the type of the implicit object parameter (arr).

* delete [] means I am deleting an array so call the desctructor of each element one by one.

# BIG THREE
* copy constructor
* destructor
* assignment operator
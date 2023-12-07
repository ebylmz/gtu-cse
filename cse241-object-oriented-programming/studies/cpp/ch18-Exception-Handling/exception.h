#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <stdexcept>

using std::runtime_error;

namespace ExceptionEBY {
    class DividedByZero : public runtime_error {
    public:
        DividedByZero () : runtime_error("attempt to divide by zero") {} 
    };
}
#endif
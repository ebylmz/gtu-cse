// This is cse241 final exam question askey by YSA
// Find the average of given vector by recursively

#include <iostream>
#include <vector>

using std::vector;

template<typename T>
T average (vector<T> v) {
    if (v.size() == 1)
        return v[0];
    else {
        T last = v[v.size() - 1]; 
        v.pop_back();
        return (last + average(v) * v.size()) / (v.size() + 1);
    }
}

int main (void) {
    vector<double> v = {1.1, 23.3, 3.0, 43.9, 3.2};

    std::cout << "average: " << average(v) << std::endl;
}
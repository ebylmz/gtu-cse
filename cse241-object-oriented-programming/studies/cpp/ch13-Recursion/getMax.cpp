// This is cse241 final exam question askey by YSA
// Find the maximım element in the given vector by recursively

#include <iostream>

template<typename T>
T getMax (const T arr[], int size) {
    if (size == 1)
        return arr[0];
    else {
        int max = getMax(arr + 1, size - 1);
        return arr[0] > max ? arr[0] : max;
    }
} 

int main (void) {
    const int ARR_SIZE = 6;
    double arr[ARR_SIZE] = {12.3, 453.3, 19.9, 32.4, 12.0, 230.4};

    for (auto v : arr)
        std::cout << v << " ";
    std::cout << std::endl;

    std::cout << "max element: " << getMax(arr, ARR_SIZE) << std::endl;
}
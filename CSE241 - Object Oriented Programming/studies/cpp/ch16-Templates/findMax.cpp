#include <iostream>
#include <ctime>
#include <cstdlib>

using std::cout;
using std::endl;
using std::invalid_argument;

// helper function for findMax
// implemented to provide tail recursion
template<typename T>
int _findMax (const T arr[], int curi, int maxi, int size) {
    if (curi == size)
        return maxi;
    else if (arr[curi] > arr[maxi])
        return _findMax(arr, curi + 1, curi, size);
    else
        return _findMax(arr, curi + 1, maxi, size);
}

template<typename T>
int findMax (const T arr[], int size) {
    if (arr == nullptr || size == 0)
        throw invalid_argument("empty container");
    return _findMax(arr, 1, 0, size);
}

int main (void) {
    const int ARR_SIZE = 30;
    int arr[ARR_SIZE];

    srand(time(NULL));

    for (int i = 0; i < ARR_SIZE; ++i)
        arr[i] = rand() % 100;
    for (int i = 0; i < ARR_SIZE; ++i)
        cout << arr[i] << " ";
    cout << endl;

    int maxi = findMax(arr, ARR_SIZE);
    cout << "max at index " << maxi << " with value " << arr[maxi] << endl; 
}
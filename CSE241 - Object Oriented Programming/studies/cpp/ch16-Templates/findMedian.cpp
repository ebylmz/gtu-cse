// cse241 final exam question 

#include <iostream>
#include <algorithm>

template<typename T>
T * findMedian (const T arr[], int size) {
    T * median = new T[2];
    bool finded = false;
    bool m1 = false;
    bool m2 = false;

    for (int i = 0; i < size && (!m1 || !m2); ++i) {
        int smaller = 0;
        int larger = 0;
        for (int j = 0; j < size; ++j)
            if (arr[i] < arr[j])
                ++smaller;
            else if (arr[i] > arr[j])
                ++larger;

        if (size % 2 == 1) {
            if (larger == smaller) {
                median[0] = median[1] = arr[i];
                finded = true;
            }
        }
        else {
            if (smaller - larger == 1) {
                median[0] = arr[i];
                m1 = true;
            }
            else if (larger - smaller == 1) {
                median[1] = arr[i];
                m2 = true;
            }
        }
    }
    return median;
}

int main (void) {
    const int SIZE = 10;
    int arr[SIZE] = {2, 13, 17, 41, 12, 24, 51, 22, 1, 79};

    int * median = findMedian(arr, SIZE);

    std::sort(arr, arr + SIZE);
    std::cout << "array content: "; 
    for (int i = 0; i < SIZE;++i)
        std::cout << arr[i] << "(" << i << ") ";
    
    std::cout << "\nmedian: " << median[0] << ", " << median[1] << std::endl;
}
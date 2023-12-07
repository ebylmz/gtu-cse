#include <iostream>
#include <ctime>
#include <cstdlib>
using std::cout;
using std::endl;

template<class T>
void insertionSort (T arr[], int size);

template<class T> 
void printArr (const T arr[], int size);

int main () {
    const int ARR_SIZE = 20;
    const int MAX_RAND_INT = 500;
    srand(time(NULL));
    
    int arr1[ARR_SIZE];
    for (int i = 0; i < ARR_SIZE; ++i)
        arr1[i] = rand() % MAX_RAND_INT;

    char arr2[ARR_SIZE];
    int charBound = 'Z' - 'A' + 1;
    for (int i = 0; i < ARR_SIZE; ++i)
        arr2[i] = rand() % charBound + 'A';

    double arr3[ARR_SIZE];
    for (int i = 0; i < ARR_SIZE; ++i)
        arr3[i] = (rand() % MAX_RAND_INT) + (rand() % MAX_RAND_INT) / 1000.00;

    printArr(arr1, ARR_SIZE);
    insertionSort(arr1, ARR_SIZE);
    printArr(arr1, ARR_SIZE);

    cout << endl;

    printArr(arr2, ARR_SIZE);
    insertionSort(arr2, ARR_SIZE);
    printArr(arr2, ARR_SIZE);

    cout << endl;

    printArr(arr3, ARR_SIZE);
    insertionSort(arr3, ARR_SIZE);
    printArr(arr3, ARR_SIZE);
}

template<class T>
void insertionSort (T arr[], int size) {
    for (int i = 1; i < size; ++i) {
        int j = i - 1;
        int key = arr[i];
        
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            --j; 
        }
        arr[j + 1] = key; 
    }
}

template<class T> 
void printArr (const T arr[], int size) {
    for (int i = 0; i < size; ++i)
        cout << arr[i] << " ";
    cout << endl; 
}
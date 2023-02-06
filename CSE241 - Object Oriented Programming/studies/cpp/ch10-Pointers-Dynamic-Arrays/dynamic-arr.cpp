#include <iostream>

using namespace std;

double getAve (const int * arr, int size);
// Calculates the average of the given int arr

int ** createArr2d (int row, int col);

void freeArr2d (int ** arr, int row);


// nullptr is C++ keyword

int main (void) {
    int size;
    cout << "How many numbers you enter? ";
    cin >> size;

    // Allocate exact memory for the dynamic array
    int * a = new int[size];

    for (int i = 0; i < size; ++i) {
        cout << ">> ";
        cin >> a[i];
    }
    cout << "Average of entered numbers: " << getAve(a, size) << endl;
    // Free the allocated memory space
    delete [] a;
}

double getAve (const int * arr, int size) {
    double sum = 0;
    double ave;
    if (size > 0) {
        for (int i = 0; i < size; ++i)
            sum += arr[i];
        ave = sum / size;
    }
    else
        ave = -1.0;
    return ave;
}

int ** createArr2d (int row, int col) {
    int ** arr = new int *[row];    
    for (int i = 0; i < row; ++i)
        arr[i] = new int[col];
    return arr;
}

void freeArr2d (int ** arr, int row) {
    for (int i = 0; i < row; ++i)
        delete [] arr[i];   // reverse of arr[i] = new int[col];
    delete [] arr;
}
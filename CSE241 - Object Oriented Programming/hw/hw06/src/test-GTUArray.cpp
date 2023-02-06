#include <iostream>
#include <algorithm>
#include <string> 
#include "../include/GTUArray.h"

using std::cout;
using std::endl;
using std::string;
using stdGTU::GTUArray;

void test1 ();

int main (void) {
    test1();
}

void test1 () {
    cout << "================== TEST 1 ==================\n";
    const int ARR_SIZE = 10;
    GTUArray<int, ARR_SIZE> arr;

    srand(time(NULL));

    for (int i = 0; i < ARR_SIZE; ++i)
        arr[i] = rand() % 100;
    
    cout << "Initial array: " << arr << endl;

    std::sort(arr.begin(), arr.end());
    cout << "Sorted array:  " << arr << endl;

    cout << "================ TEST DONE =================\n\n";
}
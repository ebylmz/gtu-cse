#include <iostream>
#include <vector>

using namespace std;

void test_array_as_argument (int * arr, int size) {
/*  this range-based 'for' statement requires a suitable "begin" function and none was found    
    for (int i : arr)  
        cout << i << endl;                              */
    for (int i = 0; i < size; ++i)
        cout << arr[i] << endl;
}

int main (void) {
    vector<int> v(20);

    for (auto & i : v) 
        i = 17;

    for (int i : v)
        cout << i << " ";
    cout << endl;

    return 0;
}
#include <iostream>
#include <string>
#include "array.h"

using std::cout;
using std::cin;
using std::endl;
using std::string;
using stdEBY::Array;

void test1 ();
void test2 ();
void test3 ();

int main () {
    test1();
    return 0;
}

void test1 () {
    const int N = 10;
    const int M = 5;
    Array<Array<int, M>, N> a;

    for (int i = 0; i < N; ++i)
        a[i].fill((i + 1) * 10);
    
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < M; ++j)
            cout << a[i][j] << " ";
        cout << endl;
    }
    cout << endl;
}

void test2 () {
    const int N = 3;
    Array<string, N> a;
    cout << "Enter " << N << " words:\n";
 
    string nextWord;
    int used;
    for (used = 0; used < N; used++) {
        cin >> nextWord;
        a[used] = nextWord;
    }

    cout << "You wrote the following:\n";
    for (int i = 0; i < used; i++)
        cout << a[i] << " ";
    cout << endl;
}


void test3 () {
    const int N = 10;
    Array<int, N> a;

    cout << "Enter up to " << N << " nonnegative integers.\n";
    cout << "Place a negative number at the end.\n";
    int next;
    cin >> next;
    int used;
    for (used = 0; next >= 0 && used <= a.size(); ++used) {
        a[used] = next;
        cin >> next;
    }

    if (next >= 0) {
        cout << "Could not read all numbers.\n";
        //Clear the unread input:
        while (next >= 0)
            cin >> next;
    }
    
    cout << "You entered the following:\n ";
    for (int i = 0; i < used; i++)
        cout << a[i] << " ";
    cout << endl;
}
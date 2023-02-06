#include <iostream>
#include <vector>
#include <ctime>

using namespace std;

void printVector (const vector<int> & v) {
    for (int i : v)
        cout << i << ' ';
    cout << endl;
}

// Finds and returns the first n different biggest number
void firstMax (const vector<int> & vin, vector<int> & vout,  int n) {
    vector<int> v(vin);

    for (int i = 0; i < n && i < v.size(); ++i) {
        // keepst the index of current max in vector v 
        int maxi = i;
        for (int j = i + 1; j < v.size(); ++j)
            if (v[j] > v[maxi])
                maxi = j;
        if (i != maxi) {
            int tmp = v[i];
            v[i] = v[maxi];
            v[maxi] = tmp;
        }

        if (i > 0 && v[i] == v[i - 1])
            ++n;
        else
            vout.push_back(v[i]);
    }
    cout << endl;
}

int main (void) {
    vector<int> vin;
    vector<int> vout;

    srand(time(NULL));
    for (int i = 0; i < 30; ++i)
        vin.push_back(rand() % 100);
    
    firstMax(vin, vout, 5);

    cout << "Vector order  : ";
    printVector(vin);
    cout << "FirstMax order: ";
    printVector(vout);

    return 0;
}
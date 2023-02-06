#include <iostream>
using namespace std;

// recursive function is tail recursive if recursive call is the last thing executed by the function. 

// non tail recursion example
//
// int fact (int n) {
//     if (n <= 1) return 1;
//     else return n * fact(n - 1);
// }

unsigned int factH (unsigned int n, unsigned int ans) {
    if (n == 1 || n == 0)
        return ans;
    else 
        return factH(n - 1, n * ans);
}

unsigned int fact (unsigned int n) {
    return factH(n, 1);
}

unsigned int strlenH (char * str, unsigned int len) {
    if (*str == '\0')
        return len;
    else
        return strlenH(str + 1, len + 1);
}
 
unsigned int strlen (char * str) {
    return strlenH(str, 0);
}

int main (void) {
    int n = 5;
    cout << n << "! = " << fact(n) << endl;

    char s[] = "emirkan";
    cout << "strlen("<< s <<") = " << strlen(s) << endl;
}
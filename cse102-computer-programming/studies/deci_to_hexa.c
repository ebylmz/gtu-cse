#include <stdio.h>
#include <stdlib.h>
#include <time.h>

char * deci_to_hexa (int deci) {
    char tmp[20], * hexa;
    int r, n = 0, i;

    do {
        r = deci % 16;
        tmp[n++] = r < 10 ? r + '0' : r - 10 + 'A';
        deci /= 16;
    } while (deci > 0);

    hexa = (char *) calloc(n + 1, sizeof(char));
    if (hexa != NULL) {
        for (i = 0, n -= 1; n >= 0; ++i, --n)
            hexa[i] = tmp[n];
        hexa[i] = '\0';
    }

    return hexa;
}

void test () {
    int i, deci;
    char * hexa; 
    
    srand(time(NULL));

    for (i = 0; i < 30; ++i) {
        deci = rand() % 1000;
        hexa = deci_to_hexa(deci);
        printf("%-3d : %s\n", deci, hexa);
        free(hexa);
    }
}

int main (void) {
    test();
} 

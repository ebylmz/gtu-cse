#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int pairsOfSumLoop(int arr[], int size, int sum) {
    int count = 0;
    for (int i = 0; i < size; ++i) {
        int target = sum - arr[i];
        for (int j = i + 1; j < size; ++j)
            if (arr[j] == target)
                ++count;
    }
    return count;
}

int pairsOfSumRecursive(int arr[], int size, int sum) {
    int count = 0;
    if (size > 0) {
        int target = sum - arr[size - 1];
        for (int i = size - 2; i >= 0; --i)
            if (arr[i] == target)
                ++count;
        count += pairsOfSumRecursive(arr, size - 1, sum);
    }
    return count;
}

int main(void) {
    const int ARR_SIZE = 1000000;
    const int sum = 10;
    int vals[] = {-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    
    int * arr = (int *) calloc(ARR_SIZE, sizeof(int));

    for (int i = 0; i < ARR_SIZE; ++i)
        arr[i] = vals[i % 20];

    for (int n = 1000; n <= ARR_SIZE; n *= 10) {
        clock_t begin = clock();
        pairsOfSumLoop(arr, n, sum); 
        // pairsOfSumRecursive(arr, 15, n - 1);
        clock_t end = clock();

        double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
        printf("Running time for problem size %-10.d: %.5f\n", n, time_spent);
    }

    free(arr);
}


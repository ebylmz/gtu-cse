#include <thread>
#include <vector>
#include "simulation.h"
#include <pthread.h>

long int simulate(MemoryManagement &mem, unsigned long int start, int numRows, int numCols)
{
    long matrix, vector, resultVector, resultMatrix, arrSum, end;
    int matrixSize, vectorSize, resultVectorSize, resultMatrixSize, arrSumSize; 
    pthread_mutex_t mutex_stdout;

    matrixSize = numRows * numCols;         /* NxM matrix */ 
    vectorSize = numCols * 1;               /* Mx1 vector */
    resultVectorSize = numRows * 1;         /* NxM * Mx1 = Mx1 vector */
    resultMatrixSize = numRows * numRows;   /* Nx1 * 1xN = NxN matrix */
    
    /* Mx1 array + NxN array = size of N*N or M array */
    arrSumSize = numRows * numRows > numCols ? numRows * numRows : numCols; 

    matrix = start;
    vector = matrix + matrixSize;
    resultVector = vector + vectorSize;
    resultMatrix = resultVector + resultVectorSize;
    arrSum = resultMatrix + resultMatrixSize;
    end = arrSum + arrSumSize;

    /* Make sure not to exceed virtual memory address space */
    if (end > mem.getVirtualMemSize())
        return -1;

    pthread_mutex_init(&mutex_stdout, NULL);

    pthread_mutex_lock(&mutex_stdout);
    std::cout << "matrix vector multiplication...\n";
    pthread_mutex_unlock(&mutex_stdout);

    std::thread t1 = std::thread(parallelMatrixVectorMultiply, std::ref(mem), matrix, vector, resultVector, numRows, numCols);

    pthread_mutex_lock(&mutex_stdout);
    std::cout << "vector transpose multiplication...\n";
    pthread_mutex_unlock(&mutex_stdout);
    std::thread t2 = std::thread(parallelVectorTransposeMultiply, std::ref(mem), vector, resultMatrix, numCols);
    
    t1.join();
    t2.join();

    pthread_mutex_lock(&mutex_stdout);
    std::cout << "array summation...\n";
    pthread_mutex_unlock(&mutex_stdout);

    arraySummation(mem, resultVector, resultVectorSize, resultMatrix, resultMatrixSize, arrSum);

    /* After the multiplications and summation print the result array */
    pthread_mutex_lock(&mutex_stdout);
    printArray(mem, arrSum, arrSumSize);
    pthread_mutex_unlock(&mutex_stdout);

    /* search 5 value, 2 of them not in the mem */
    int targets[] = {1000, 24, -24, 75, 34}, numTargets = 5;
    int result;
    
    /* Linear search on result array */
    pthread_mutex_lock(&mutex_stdout);
    std::cout << "Linear search..." << std::endl; 
    pthread_mutex_unlock(&mutex_stdout);
    for (int i = 0; i < numTargets; ++i) {
        result = linearSearch(mem, arrSum, arrSumSize, targets[i]);
        pthread_mutex_lock(&mutex_stdout);
        std::cout << "target: " << targets[i] << "\tresult: " << result << std::endl; 
        pthread_mutex_unlock(&mutex_stdout);
    }

    /* First sort the array then apply binary search */
/*
    // std::cout << "Quick sort...." << std::endl; 
    // quickSort(mem, arrSum, 0, arrSumSize - 1);

    pthread_mutex_lock(&mutex_stdout);
    std::cout << "Binary search...." << std::endl; 
    for (int i = 0; i < numTargets; ++i) {
        result = binarySearch(mem, arrSum, arrSumSize, targets[i]);
        std::cout << "target: " << targets[i] << "\tresult: " << result << std::endl; 
    }
    pthread_mutex_unlock(&mutex_stdout);
*/

    pthread_mutex_destroy(&mutex_stdout);

    /* Return the next simulation address */
    return end;
}

void simpleMultiplication(MemoryManagement &mem)
{

    long matrix, vector, resultVector, resultMatrix;
    int numRows, numCols, matrixSize, vectorSize, resultVectorSize;
    int resultVectorRow, resultVectorCol, resultMatrixRow, resultMatrixCol;   
   
    numRows = 5;
    numCols = 3;

    matrixSize = numRows * numCols; 
    vectorSize = numCols * 1;
    resultVectorRow = numRows; /* 5x3 * 3x1 = 5x1 */
    resultVectorCol = 1;
    resultMatrixRow = numCols; /* 3x1 * 1x3 = 3x3 */
    resultMatrixCol = numCols;
    resultVectorSize = resultVectorRow * resultVectorCol;
    // resultMatrixSize = resultMatrixRow * resultMatrixCol;
    
    matrix = 0;
    vector = matrix + matrixSize;
    resultVector = vector + vectorSize;
    resultMatrix = resultVector + resultVectorSize;

    std::cout << "matrix (" << numRows << "x" << numCols <<  ")\n";
    printMatrix(mem, 0, numRows, numCols);
    std::cout << std::endl;
    std::cout << "vector (" << vectorSize << "x1)\n";
    printMatrix(mem, matrixSize, vectorSize, 1);
    std::cout << std::endl;

    parallelMatrixVectorMultiply(mem, matrix, vector, resultVector, numRows, numCols);

    std::cout << "result vector (" << resultVectorRow << "x" << resultVectorCol << ")\n";
    printMatrix(mem, resultVector, resultVectorRow, resultVectorCol);
    std::cout << std::endl;

    parallelVectorTransposeMultiply(mem, vector, resultMatrix, vectorSize);

    std::cout << "result matrix (" << resultMatrixRow << "x" << resultMatrixCol << ")\n";
    printMatrix(mem, resultMatrix, numCols, numCols);
    std::cout << std::endl;
}

unsigned long int calcSimulationAdressSpace(int numRows, int numCols)
{
    unsigned long int matrixSize, vectorSize, resultVectorSize, resultMatrixSize, arrSumSize; 
    matrixSize = numRows * numCols;         /* NxM matrix */ 
    vectorSize = numCols * 1;               /* Mx1 vector */
    resultVectorSize = numRows * 1;         /* NxM * Mx1 = Mx1 vector */
    resultMatrixSize = numRows * numRows;   /* Nx1 * 1xN = NxN matrix */
    arrSumSize = numRows * numRows > numCols ? numRows * numRows : numCols; 
    return matrixSize + vectorSize + resultVectorSize + resultMatrixSize + arrSumSize;
}

void printMatrix(MemoryManagement &mem, unsigned long int matrix, int numRows, int numCols)
{
    int cell;
    
    for (int i = 0; i < numRows; ++i) {
        for (int j = 0; j < numCols; ++j) {
            cell = mem.read(matrix);
            std::cout << cell << " ";
            ++matrix;
        }
        std::cout << std::endl;
    }
}

void printArray(MemoryManagement &mem, unsigned long int arr, int size)
{
    int val;
    
    for (int i = 0; i < size; ++i) {
        val = mem.read(arr + i);
        std::cout << val << " ";
    }
    std::cout << std::endl;
}


void matrixVectorMultiply(MemoryManagement &mem, unsigned long int matrix, unsigned long int vector, unsigned long int result, int numCols, int startRow, int endRow)
{    
    int matrixCell, vectorCell, rowSum;
    matrix += startRow * numCols;
    for (int row = startRow; row < endRow; ++row) {
        rowSum = 0;
        for (int col = 0; col < numCols; ++col) {
            /* result[row] += matrix[row][col] * vector[col]; */
            matrixCell = mem.read(matrix); // matrix + row * numCols + col
            vectorCell = mem.read(vector + col);
            rowSum += matrixCell * vectorCell;
            ++matrix;
        }
        #ifdef DEBUG
        std::cerr << "[" << result + row << "] = " << rowSum << std::endl
        #endif
        /* Write the result */
        mem.write(result + row, rowSum);
    }
}

void parallelMatrixVectorMultiply(MemoryManagement &mem, unsigned long int matrix, unsigned long int vector, unsigned long int result, int numRows, int numCols)
{
    int rowsPerThread = 1;

/*
    if (numRows < 10)
        rowsPerThread = 1;
    else if (numRows < 100)
        rowsPerThread = 10;
    else if (numRows < 1000)
        rowsPerThread = 100;
*/

    int numThreads = numRows / rowsPerThread;

    if (numThreads == 0)
        numThreads = numRows;

    std::vector<std::thread> threads;
    int startRow = 0;
    int endRow = 0;
    
    for (int i = 0; i < numThreads - 1; ++i) {
        startRow = i * rowsPerThread;
        endRow = startRow + rowsPerThread;
        threads.emplace_back(matrixVectorMultiply, std::ref(mem), matrix, vector, result, numCols, startRow, endRow);
    }    

    /* Assign the remaining rows to the last thread */
    startRow = (numThreads - 1) * rowsPerThread;
    endRow = numRows;
    threads.emplace_back(matrixVectorMultiply, std::ref(mem), matrix, vector, result, numCols, startRow, endRow);

    for (auto &thread : threads)
        thread.join();
}

void vectorTransposeMultiply(MemoryManagement &mem, unsigned long int vector, int vectorSize, unsigned long int result, int startRow, int endRow) 
{
    int vectorCell, tranposeCell;
    int numCols = vectorSize; 
    
    result = result + startRow * numCols; 
    
    /* startRow and endRow are given for result matrix */
    for (int row = startRow; row < endRow; ++row) {
        /* result[row][col] = vector[row] * vector[col] */
        vectorCell = mem.read(vector + row); 
        for (int col = 0; col < numCols; ++col) {
            tranposeCell = mem.read(vector + col);
            mem.write(result, vectorCell * tranposeCell);
            ++result;           
        }
    }
}

void parallelVectorTransposeMultiply(MemoryManagement &mem, unsigned long int vector, unsigned long int result, int vectorSize)
{
    int rowsPerThread = 1;

/*
    if (vectorSize < 10)
        rowsPerThread = 1;
    else if (vectorSize < 100)
        rowsPerThread = 10;
    else if (vectorSize < 1000)
        rowsPerThread = 100;
*/

    int numThreads = vectorSize / rowsPerThread;
    if (numThreads == 0)
        numThreads = vectorSize;

    std::vector<std::thread> threads;
    int startRow = 0;
    int endRow = 0;

    for (int i = 0; i < numThreads - 1; ++i) {
        startRow = i * rowsPerThread;
        endRow = startRow + rowsPerThread;
        if (endRow > vectorSize) {
            endRow = vectorSize;
        }
        threads.emplace_back(vectorTransposeMultiply, std::ref(mem), vector, vectorSize, result, startRow, endRow);
    }

    /* Assign the remaining rows to the last thread */
    startRow = (numThreads - 1) * rowsPerThread;
    endRow = vectorSize;
    threads.emplace_back(vectorTransposeMultiply, std::ref(mem), vector, vectorSize, result, startRow, endRow);

    for (auto &thread : threads)
        thread.join();
}

int arraySummation(MemoryManagement &mem, unsigned long int arr1, int size1, unsigned long int arr2, int size2, unsigned long int result)
{
    int sizeSmall, sizeLarge;
    unsigned long int arrLarge;

    if (size1 < size2) {
        sizeSmall = size1;
        sizeLarge = size2;
        arrLarge = arr2; 
    }
    else {
        sizeSmall = size2;
        sizeLarge = size1;
        arrLarge = arr1; 
    }  

    int i = 0;
    while (i < sizeSmall) {
        mem.write(result + i, mem.read(arr1 + i) + mem.read(arr2 + i));
        ++i;
    }

    while (i < sizeLarge) {
        mem.write(result + i, mem.read(arrLarge + i));
        ++i;
    }

    /* return the size of the result array */
    return i;
}

int arraySummation(MemoryManagement &mem, unsigned long int arr, int size)
{
    int sum = 0;
    for (int i = 0; i < size; ++i)
        sum += mem.read(arr + i);
    return sum;
}

int linearSearch(MemoryManagement &mem, unsigned long int arr, int size, int target)
{
    for (int i = 0; i < size; ++i)
        if (mem.read(arr + i) == target)
            return i;
    return -1;
}

void swap(MemoryManagement &mem, unsigned long int a, unsigned long int b)
{
    int tmp = mem.read(a);
    mem.write(a, mem.read(b));
    mem.write(b, tmp);
}
 
int partition(MemoryManagement &mem, unsigned long int arr, int low, int high)
{
    // Choosing the pivot
    int pivot = mem.read(arr + high);
 
    // Index of smaller element and indicates
    // the right position of pivot found so far
    int i = (low - 1);
 
    for (int j = low; j <= high - 1; j++) {
 
        // If current element is smaller than the pivot
        if (mem.read(arr + j) < pivot) {
 
            // Increment index of smaller element
            i++;
            swap(mem, arr + i, arr + j);
        }
    }
    swap(mem, arr + i, arr + high);
    return (i + 1);
}
 
void quickSort(MemoryManagement &mem, unsigned long int arr, int low, int high)
{
    if (low < high) {
        // pi is partitioning index, arr[p]
        // is now at right place
        int pi = partition(mem, arr, low, high);
 
        // Separately sort elements before
        // partition and after partition
        quickSort(mem, arr, low, pi - 1);
        quickSort(mem, arr, pi + 1, high);
    }
}

void insertionSort(MemoryManagement &mem, unsigned long int arr, int size) 
{
    for (int i = 1; i < size; ++i) {
        int j = i - 1;
        int key = mem.read(arr + i);
        
        int curr;
        while (j >= 0 && (curr = mem.read(arr + j) > key)) {
            mem.write(arr + j + 1, curr);
            --j;
        }
        if (j < i - 1)
            mem.write(arr + j + 1, key);
    }
}

int binarySearch(MemoryManagement &mem, unsigned long int arr, int size, int target)
{
    int left = 0;
    int right = size - 1;

    while (left <= right) {
        int mid = left + (right - left) / 2;
        int midval = mem.read(arr + mid);
        if (midval == target)
            return mid;
        else if (target < midval)
            right = mid - 1;
        else 
            left = mid + 1;
    }

    return -1;
}
#ifndef SIMULATION_H
#define SIMULATION_H

#include "memory.h"

#define SIM_MATRIX_ROW 100
#define SIM_MATRIX_COL 3

long int simulate(MemoryManagement &mem, unsigned long int start, int numRows, int numCols);

void simpleMultiplication(MemoryManagement &mem);

unsigned long int calcSimulationAdressSpace(int numRows, int numCols);

void printMatrix(MemoryManagement &mem, unsigned long int matrix, int numRows, int numCols);

void printArray(MemoryManagement &mem, unsigned long int arr, int size);

void matrixVectorMultiply(MemoryManagement &mem, unsigned long int matrix, unsigned long int vector, unsigned long int result, int numCols, int startRow, int endRow);

void parallelMatrixVectorMultiply(MemoryManagement &mem, unsigned long int matrix, unsigned long int vector, unsigned long int result, int numRows, int numCols);

void vectorTransposeMultiply(MemoryManagement &mem, unsigned long int vector, int vectorSize, unsigned long int result, int startRow, int endRow);

void parallelVectorTransposeMultiply(MemoryManagement &mem, unsigned long int vector, unsigned long int result, int vectorSize);

int arraySummation(MemoryManagement &mem, unsigned long int arr, int size);

int arraySummation(MemoryManagement &mem, unsigned long int arr1, int size1, unsigned long int arr2, int size2, unsigned long int result);

void swap(MemoryManagement &mem, unsigned long int a, unsigned long int b);

int partition(MemoryManagement &mem, unsigned long int arr, int low, int high);

void quickSort(MemoryManagement &mem, unsigned long int arr, int low, int high);

int linearSearch(MemoryManagement &mem, unsigned long int arr, int size, int target);

void insertionSort(MemoryManagement &mem, unsigned long int arr, int size);

int binarySearch(MemoryManagement &mem, unsigned long int arr, int size, int target);

int print_threadsafe(int fd, pthread_mutex_t *fd_mutex, const char *format, ...);

#endif
#ifndef __UTILITY_H
#define __UTILITY_H

#include <common/types.h>

namespace myos
{
    void printf(char *str);
    void printfHex(common::uint8_t key);
    int scanf(char *out);
    int scanfPrompt(char *prompt, int *out);
    void sleep(int seconds);
    int atoi(char *str, int *out);
    void printDigit(int digit); 
    void printArr(int *arr, int size);
    int rand(int min, int max);
    void swap(int *a, int *b); 
    int partition(int arr[], int low, int high); 
    void quickSort(int *arr, int low, int high); 
}

#endif
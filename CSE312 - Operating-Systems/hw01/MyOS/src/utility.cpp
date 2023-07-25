
#include <common/types.h>
#include <drivers/keyboard.h>
#include <drivers/mouse.h>
#include <hardwarecommunication/interrupts.h>
#include <utility.h>

using namespace myos;
using namespace myos::common;
using namespace myos::drivers;
using namespace myos::hardwarecommunication;

namespace myos {
    void printf(char* str)
    {
        static common::uint16_t* VideoMemory = (common::uint16_t*)0xb8000;

        static common::uint8_t x=0,y=0;

        for(int i = 0; str[i] != '\0'; ++i)
        {
            switch(str[i])
            {
                case '\n':
                    x = 0;
                    y++;
                    break;
                default:
                    VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0xFF00) | str[i];
                    x++;
                    break;
            }

            if(x >= 80)
            {
                x = 0;
                y++;
            }

            if(y >= 25)
            {
                for(y = 0; y < 25; y++)
                    for(x = 0; x < 80; x++)
                        VideoMemory[80*y+x] = (VideoMemory[80*y+x] & 0xFF00) | ' ';
                x = 0;
                y = 0;
            }
        }
    }

    void printfHex(common::uint8_t key)
    {
        char* foo = "00";
        char* hex = "0123456789ABCDEF";
        foo[0] = hex[(key >> 4) & 0xF];
        foo[1] = hex[key & 0xF];
        printf(foo);
    }

    // TODO: paramater max that indicates number of charachter being read
    int scanf(char *out) {
        char *str;
        int i;
        InterruptManager::setIOLock(true); 
        // read the whole line
        while ((str = InterruptManager::stdin.read('\n')) == nullptr) ; /* busy waiting */
        
        for (i = 0; str[i] != '\0' && str[i] != '\n'; ++i)
            out[i] = str[i];
        out[i] = '\0';
        InterruptManager::setIOLock(false);
        return i; // return number of charachters being read
    }

    int scanfPrompt(char *prompt, int *out) {
        int rv;
        char buff[128];

        printf(prompt);
        rv = scanf(buff);
        while (atoi(buff, out) == -1) {
            printf("Invalid format error\n");
            printf(prompt);
            rv = scanf(buff);
        }
        return rv;
    }

    void sleep(int seconds) 
    {
        long int delay = 100000000; // 1 second delay
        // Loop until the desired amount of time has passed
        for (long int i = 0; i < seconds * delay; ++i)
            i++;
    }

    int atoi(char *str, int *out)
    {
        char *start = str;
        int sign;
        int num = 0;
        
        // sign check
        if ((sign = (str[0] == '-' || str[0] == '+')))
            ++str;

        while (*str != '\0') {
            if (*str < '0' || '9' < *str)
                return -1;
            num =  num * 10 + (*str - '0');
            ++str; 
        }
        *out = sign ? (((start[0] == '-') ? -1 : 1) * num) : (num);
        return 0;
    }

    void printDigit(int digit) 
    {
        char buff[256];
        int n; 
        int i;
        
        // check if the digit is positive or negative
        if (digit < 0) {
            digit *= -1;
            buff[0] = '-';
            i = n = 1;
        }
        else {
            i = n = 0;
        }

        do {
            buff[n] = '0' + (digit % 10);
            digit /= 10;
            ++n;
        } while (digit > 0);

        buff[n] = '\0';
        
        while (i < n / 2) {
            int temp = buff[i];
            buff[i] = buff[n - i - 1];
            buff[n - i - 1] = temp;
            ++i;        
        }
        printf((char *) buff);
    }

    void printArr(int *arr, int size)
    {
        for (int i = 0; i < size; ++i) {
            printDigit(arr[i]); 
            printf(" ");
        }
        printf("\n");
    }

    int32_t rand(int min, int max) 
    {
        uint64_t counter;
        int32_t num;
        /* Read the clock counter */
        asm("rdtsc": "=A"(counter));

        /* Use the clock counter as a source of randomness */
        counter = counter * 1103515245 + 12345;
        num = (int)(counter / 65536) % (max - min);
        if (num < 0)
            num += max;
        return num + min;
    }

    void swap(int* a, int* b) 
    {
        int t = *a;
        *a = *b;
        *b = t;
    }
    
    int partition(int arr[], int low, int high) 
    {
        int pivot = arr[high];
        int i = (low - 1);
    
        for (int j = low; j <= high - 1; j++) {
            if (arr[j] < pivot) {
                i++;
                swap(&arr[i], &arr[j]);
            }
        }
        swap(&arr[i + 1], &arr[high]);
        return (i + 1);
    }
    
    void quickSort(int * arr, int low, int high) 
    {
        if (low < high) {
            int pi = partition(arr, low, high);
            quickSort(arr, low, pi - 1);
            quickSort(arr, pi + 1, high);
        }
    }
}
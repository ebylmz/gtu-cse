#include <utility.h>
#include <gdt.h>
#include <syscalls.h>
#include <multitasking.h>
#include <test.h>

namespace myos {
    void epTestFork() 
    {
        for (int i = 0; i < 50; ++i) {
            printDigit(i); 
            printf(" ");
        }
        printf("\n");
        SyscallHandler::sys_fork();
        printf("Helloo from fork\n"); 
        while (true) ; // infinitive loop
    }

    void testFork(GlobalDescriptorTable *gdt, TaskManager *taskManager) 
    {
        Task taskFork(gdt, epTestFork, 0, Normal);
        taskManager->AddTask(&taskFork);
    }

    void epTestScanf() 
    {
        int age;
        scanfPrompt("How old are you? ", &age);
        printf("Your age is "); printDigit(age); printf("\n");
        SyscallHandler::sys_exit();
    }

    void testScanf(GlobalDescriptorTable *gdt, TaskManager *taskManager) 
    {
        Task taskScanf(gdt, epTestScanf, 0, Normal);
        taskManager->AddTask(&taskScanf);
    }

    void epTestExecve() 
    {
        for (int i = 0; i < 50; ++i) {
            printDigit(i); 
            printf(" ");
        }
        printf("\n");
        SyscallHandler::sys_execve(epCollatz);
        printf("This will not be executed\n"); 
        while (true) ; // infinitive loop
    }

    void testExecve(GlobalDescriptorTable *gdt, TaskManager *taskManager) 
    {
        Task taskExecve(gdt, epTestExecve, 0, Normal);
        taskManager->AddTask(&taskExecve);
    }

    void epTestForkAndExecve() 
    {
        for (int i = 0; i < 50; ++i) {
            printDigit(i); 
            printf(" ");
        }
        printf("\n");
        SyscallHandler::sys_fork();
        printf("Helloo from fork\n"); 
        SyscallHandler::sys_execve(epBinarySearch);
        while (true) ; // infinitive loop
    }

    void testForkAndExecve(GlobalDescriptorTable *gdt, TaskManager *taskManager) 
    {
        Task taskForkAndExecve(gdt, epTestForkAndExecve, 0, Normal);
        taskManager->AddTask(&taskForkAndExecve);
    }

    void testWaitpid(GlobalDescriptorTable *gdt, TaskManager *taskManager) 
    {
        // uncomment waitpid sentence from the epLinearSearch function
        lifecyle1(gdt, taskManager);
    }

    void collatzSeq(int n, int * buff) 
    {
        int i = 0;
        buff[0] = n;
        while (n > 1) {
            n = (n % 2 == 0) ? n / 2 : 3 * n + 1; 
            buff[++i] = n;
        }
    }

    void printSeq(int * seq) 
    {
        printDigit(*seq); printf(": ");
        while (*(++seq) > 1) {
            printDigit(*seq); printf(" ");
        }

        printDigit(1); printf("\n");
    }

    void epCollatz() 
    {
        int str[64];
        int buff[256];
        int n;

        printf("program: Collatz\n");
        
        scanfPrompt("sequence number: ", &n);

        printf("Collatz("); printDigit(n); printf("):\n");

        for (int i = n; i > 0;--i) {
            collatzSeq(i, buff);
            printSeq(buff);
        }
        SyscallHandler::sys_exit();
    }

    int linearSearch(int * arr, int size, int target) 
    {
        for (int i = 0; i < size; ++i)
            if (arr[i] == target) 
            return i;
        return -1;
    }

    void epLinearSearch() 
    {
        int arr[] = {10, 20, 80, 30, 60, 50, 110, 100, 130, 170};
        int size = sizeof(arr) / sizeof(int);
        int target;

        // SyscallHandler::sys_waitpid(4);
        printf("program: LinearSearch\n");

        printf("array : "); printArr(arr, size);
        scanfPrompt("target value: ", &target);
        
        int i = linearSearch(arr, size, target);
        printf("LinearSeach("); printDigit(target); printf("): "); printDigit(i); printf("\n");
        sleep(5);
        SyscallHandler::sys_exit();
    }

    int binarySearch(int *arr, int target, int lo, int hi) 
    {
        if (lo > hi)
            return -1;

        int mid = (hi + lo) / 2;
        if (arr[mid] == target)
            return mid;
        else if (target < arr[mid])
            return binarySearch(arr, target, lo, mid - 1);
        else
            return binarySearch(arr, target, mid + 1, hi);
    }

    void epBinarySearch() 
    {
        int arr[] = {10, 20, 80, 30, 60, 50, 110, 100, 130, 170};
        int size = sizeof(arr) / sizeof(int);
        int target;

        printf("program: BinarySearch\n");

        printf("array: "); printArr(arr, size);

        // first sort and print the array
        quickSort(arr, 0, size - 1);
        printf("sorted array: "); printArr(arr, size);

        scanfPrompt("target value: ", &target);

        int i = binarySearch(arr, target, 0, size - 1);
        printf("BinarySeach("); printDigit(target); printf("): "); printDigit(i); printf("\n");
        sleep(5);
        SyscallHandler::sys_exit();
    }

    void lifecyle1(GlobalDescriptorTable *gdt, TaskManager *taskManager)
    {
        Task taskLinearSearch(gdt, epLinearSearch, 1, Normal);
        Task taskBinarySearch(gdt, epBinarySearch, 1, Normal);
        Task taskCollatz(gdt, epCollatz, 2, Normal);

        taskManager->AddTask(&taskLinearSearch);
        taskManager->AddTask(&taskBinarySearch);
        taskManager->AddTask(&taskCollatz);
    }

    void lifecyle2(GlobalDescriptorTable *gdt, TaskManager *taskManager)
    {
        const int NUM_OF_EXEC = 10;
        const int NUM_OF_PROG = 3;
        void (*prog[NUM_OF_PROG]) (void) = {epLinearSearch, epBinarySearch, epCollatz};

        void (*entrypoint)(void) = prog[rand(0, NUM_OF_PROG)];

        Task task;

        // load randomly selected program in specified number of times
        for (int i = 0; i < NUM_OF_EXEC; ++i) {
            task.InitTask(gdt, entrypoint, 1, 1, Normal);
            taskManager->AddTask(&task);
        }
    }

    void lifecyle3(GlobalDescriptorTable *gdt, TaskManager *taskManager)
    {
        const int NUM_OF_EXEC = 3;
        const int NUM_OF_PROG = 3;
        void (*prog[NUM_OF_PROG]) (void) = {epLinearSearch, epBinarySearch, epCollatz};

        int r1 = rand(0, NUM_OF_PROG), r2 = rand(0, NUM_OF_PROG);
        while (r1 == r2) 
            r2 = rand(0, NUM_OF_PROG);
        
        Task task;

        // load randomly selected 2 program in specified number of times
        for (int i = 0; i < NUM_OF_EXEC; ++i) {
            task.InitTask(gdt, prog[r1], 1, 1, Normal);
            taskManager->AddTask(&task);
            task.InitTask(gdt, prog[r2], 1, 1, Normal);
            taskManager->AddTask(&task);
        }
    }
}
#ifndef __TEST_H
#define __TEST_H

#include <gdt.h>
#include <multitasking.h>

namespace myos
{
    /* ep denotes entrypoint */
    
    void lifecyle1(GlobalDescriptorTable *gdt, TaskManager *taskManager);
    void lifecyle2(GlobalDescriptorTable *gdt, TaskManager *taskManager);
    void lifecyle3(GlobalDescriptorTable *gdt, TaskManager *taskManager);

    int binarySearch(int *arr, int target, int lo, int hi);
    void epBinarySearch(); 

    int linearSearch(int * arr, int size, int target);
    void epLinearSearch();

    void printSeq(int *seq); 
    void collatzSeq(int n, int *buff); 
    void epCollatz(); 

    void testWaitpid(GlobalDescriptorTable *gdt, TaskManager *taskManager); 
    
    void testFork(GlobalDescriptorTable *gdt, TaskManager *taskManager); 
    void epTestFork();
    
    void testExecve(GlobalDescriptorTable *gdt, TaskManager *taskManager); 
    void epTestExecve();
    
    void testForkAndExecve(GlobalDescriptorTable *gdt, TaskManager *taskManager); 
    void epTestForkAndExecve();
    
    void testScanf(GlobalDescriptorTable *gdt, TaskManager *taskManager);
    void epTestScanf();
}
#endif
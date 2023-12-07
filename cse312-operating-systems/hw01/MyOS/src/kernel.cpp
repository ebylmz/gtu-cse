#include <common/types.h>
#include <utility.h>
#include <gdt.h>
#include <hardwarecommunication/interrupts.h>
#include <syscalls.h>
#include <drivers/driver.h>
#include <drivers/keyboard.h>
#include <drivers/mouse.h>
#include <multitasking.h>
#include <test.h>

using namespace myos;
using namespace myos::common;
using namespace myos::drivers;
using namespace myos::hardwarecommunication;

typedef void (*constructor)();
extern "C" constructor start_ctors;
extern "C" constructor end_ctors;
extern "C" void callConstructors()
{
    for(constructor* i = &start_ctors; i != &end_ctors; i++)
        (*i)();
}

void initProcess(GlobalDescriptorTable *gdt, TaskManager *taskManager);

extern "C" void kernelMain(const void* multiboot_structure, uint32_t /*multiboot_magic*/)
{
    GlobalDescriptorTable gdt;
    TaskManager taskManager(&gdt);

    InterruptManager interrupts(0x20, &gdt, &taskManager);
    SyscallHandler syscalls(&taskManager, &interrupts, 0x80);
    ProcessExecutionHandler execs(&taskManager, &interrupts, 0x06);

    /* Mouse and keyboard drivers */
    DriverManager drvManager;

    PrintfKeyboardEventHandler kbhandler;
    KeyboardDriver keyboard(&interrupts, &kbhandler);
    drvManager.AddDriver(&keyboard);

    MouseToConsole mousehandler;
    MouseDriver mouse(&interrupts, &mousehandler);
    drvManager.AddDriver(&mouse);
    
    drvManager.ActivateAll();

    /* Create init process */
    initProcess(&gdt, &taskManager);

    interrupts.Activate();  

    while(true);    /* kernel always runs */
}

void entrypointInit() 
{
    while (true); /* empty loop */
} 

void initProcess(GlobalDescriptorTable *gdt, TaskManager *taskManager) 
{
    printf("Welcome to MyOS\n");

    Task taskInit(gdt, entrypointInit, 0, High);
    taskManager->AddTask(&taskInit);

    lifecyle1(gdt, taskManager);

    // lifecyle2(gdt, taskManager);

    // lifecyle3(gdt, taskManager);

    // testFork(gdt, taskManager);

    // testExecve(gdt, taskManager); 

    // testForkAndExecve(gdt, taskManager); 

    // testScanf(gdt, taskManager); 

    // testWaitpid(gdt, taskManager);
}